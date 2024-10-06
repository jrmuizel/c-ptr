
use std::{alloc::Layout, cell::Cell, collections::BTreeMap, ffi::{c_int, c_void}, ops::{Deref, Index, Sub}, ptr::NonNull, rc::Rc, sync::{atomic::{AtomicU8, Ordering}, Mutex, MutexGuard}};

use libc::strlen;
use memoffset::offset_of;
use once_cell::sync::Lazy;
pub use c_ptr_derive::TypeDesc;

pub mod libc;


//use crate::list::do_sum;


//mod list;
#[derive(Default)]
struct Foo {
    x: Cell<i32>,
    y: Cell<i32>
}

#[derive(Debug)]
struct Metadata {
    cnt: Cell<usize>,
    valid: bool,
    size: usize,
    type_info: Vec<TypeInfo>
}

enum MetadataState {
    Matches,
    Uninit,
    Mismatch
}


impl Metadata {
    fn inc_ref(&self) {
        self.cnt.set(self.cnt.get() + 1);
    }

    fn matches_type<U: 'static + TypeDesc>(&mut self, offset: usize) -> MetadataState {
        eprintln!("trying to match {} at offset {}", std::any::type_name::<U>(), offset);
        //dbg!(std::any::type_name::<Cell<i32>>());
        let target_type = std::any::TypeId::of::<U>();
        //dbg!(&self.type_info, target_type, std::any::TypeId::of::<Cell<i32>>());
        if false && self.type_info.is_empty() && offset + std::mem::size_of::<U>() <= self.size {
            // the memory is currently untyped, so copy in the type desc for the
            // target type
            for ty in U::type_desc() {
                self.type_info.push(TypeInfo { ty: ty.ty, drop_ptr: ty.drop_ptr, set_ptr: ty.set_ptr, offset: ty.offset + offset, size: ty.size, name: ty.name})
            }
            eprintln!("matches because uninit");
            return MetadataState::Uninit;
        } else {
            let mut last_offset = 0;
            let mut end = 0;
            let mut i = 0;
            for _ in 0..self.type_info.len() {
                let ty = &self.type_info[i];
                assert!(last_offset <= ty.offset);
                last_offset = ty.offset;
                if ty.offset < offset {
                    end = ty.offset + ty.size;
                    i += 1;
                    continue
                }
                if ty.offset == offset {
                    if target_type == ty.ty {
                        eprintln!("found match");
                        return MetadataState::Matches;
                    }
                    end = ty.offset + ty.size;
                    i += 1;
                    continue
                }
                break;

            }
            if end <= offset && !(i < self.type_info.len() && self.type_info[i].offset < offset + std::mem::size_of::<U>()) {
                let mut desc = Vec::new();
                for ty in U::type_desc() {
                    desc.push(TypeInfo { ty: ty.ty, drop_ptr: ty.drop_ptr, set_ptr: ty.set_ptr, offset: ty.offset + offset, size: ty.size, name: ty.name})
                }
                self.type_info.splice(i..i, desc);
                return MetadataState::Uninit;
            }
            eprintln!("no match");
            return MetadataState::Mismatch;
        }
    } 
}

struct FatPtr<T> {
    ptr: NonNull<T>,
    metadata: NonNull<Metadata>
}
pub struct Ptr<T> {
    ptr: *const T
}

impl<T> Ptr<T> {

    pub fn ptr_eq(this: &Ptr<T>, other: &Ptr<T>) -> bool {
        this.ptr == other.ptr
    }

    pub const fn null() -> Ptr<T> {
        Ptr { ptr: std::ptr::null() }
    }

    pub fn raw_untyped(&self) -> *const c_void {
        self.ptr as *const c_void 
    }

    pub fn is_null(&self) -> bool {
        self.ptr == std::ptr::null()
    }

    pub fn into_void(self) -> Ptr<c_void> {
        let ptr = self.ptr;
        // we don't need to change the refcnt.
        std::mem::forget(self);
        Ptr { ptr: ptr as *const c_void }
    }

    pub fn count(&self) -> usize {
        let mut guard = METADATA_STORE.data.lock().unwrap();
        let (_, md) = METADATA_STORE.get_mut(self.ptr as *const c_void, &mut guard).expect("pointer should have metadata");
        md.cnt.get()
    }

    pub fn offset_from(&self, origin: Ptr<T>) -> isize {
        (self.ptr as isize - origin.ptr as isize) / (std::mem::size_of::<T>() as isize)
    }
}

impl<T: Default + TypeDesc + 'static> Index<isize> for Ptr<T> {
    type Output = T;

    fn index(&self, index: isize) -> &Self::Output {
        dbg!("indexing");
        let ptr = self.ptr.wrapping_offset(index);
        dbg!(std::any::type_name::<T>());
        let mut guard = METADATA_STORE.data.lock().unwrap();
        dbg!(ptr as *const _);
        let (base, md) = METADATA_STORE.get_mut(ptr as *const _, &mut guard).unwrap();
        let offset = ptr as *const _ as usize - base as usize;
        assert!(!md.type_info.is_empty());

        match md.matches_type::<T>(offset) {
            MetadataState::Matches => {},
            MetadataState::Uninit => {
                unsafe { std::ptr::write(ptr as *mut T, Default::default()); } 
            },
            MetadataState::Mismatch => {
                // drop the mutex guard so we don't poison it
                let msg = format!("Type mismatch for offset: {}, found: {}", offset, md.type_info[0].name);
                drop(guard);
                panic!("{}", msg);
            }
        }
        if !md.valid {
            panic!();
        }
        unsafe { &*ptr }
    }
}

impl<T> Default for Ptr<T> {
    fn default() -> Self {
        Self { ptr: std::ptr::null() }
    }
}

impl<T> Clone for FatPtr<T> {
    fn clone(&self) -> Self {
        unsafe { self.metadata.as_ref().inc_ref() };
        FatPtr { ptr: self.ptr, metadata: self.metadata }
    }
}

impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        if self.is_null() {
            return Self::null();
        }
        let mut guard = METADATA_STORE.data.lock().unwrap();
        let (_base, md) = METADATA_STORE.get_mut(self.ptr as *const _, &mut guard).expect("cloning pointer without metadata");
        md.inc_ref();
        Self { ptr: self.ptr }
    }
}

impl<T> Drop for Ptr<T> {
    fn drop(&mut self) {
        eprintln!("drop({:?})", self.ptr);
        if self.ptr == std::ptr::null() {
            return;
        }
        let mut guard = METADATA_STORE.data.lock().unwrap();
        let (base, md) = METADATA_STORE.get_mut(self.ptr as *const _, &mut guard).expect("pointer should have metadata");
        md.cnt.set(md.cnt.get() - 1);
        eprintln!("dropping {:?} to {}", self.ptr, md.cnt.get());
        if md.cnt.get() == 0 {
            eprintln!("dealloc {:?}", self.ptr);
            let valid = md.valid;
            unsafe { std::alloc::dealloc(base as *mut u8, Layout::from_size_align(md.size, MAX_ALIGN).unwrap()) }
            METADATA_STORE.remove(base, &mut guard);
            assert!(!valid, "contents would've leaked");
        }
    }
}

// This is likely not safe because of the generic liftetime
unsafe fn raw_deref<'a, T>(ptr: *const T) -> &'a T {
    assert_ne!(ptr, std::ptr::null());
    let mut guard = METADATA_STORE.data.lock().unwrap();
    let (base, md) = METADATA_STORE.get_mut(ptr as *const _, &mut guard).unwrap();
    if !md.valid {
        drop(guard);
        panic!("deref of freed memory");
    }
    let offset = ptr as *const _ as usize - base as usize;
    if offset == md.size {
        drop(guard);
        panic!("deref at end of object");
    }
    unsafe { &*ptr }
}


impl<T> Deref for Ptr<T> {

    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { raw_deref(self.ptr) }
    }
}

impl<T: 'static + TypeDesc> Ptr<T> {
    pub fn cast<U: 'static + TypeDesc + Default>(self) -> Ptr<U> {
        dbg!(self.ptr);
        let mut guard = METADATA_STORE.data.lock().unwrap();
        let (base, md) = METADATA_STORE.get_mut(self.ptr as *const _, &mut guard).unwrap();
        let offset = self.ptr as usize - base as usize;
        match md.matches_type::<U>(offset) {
            MetadataState::Matches => {},
            MetadataState::Uninit => {
                unsafe { std::ptr::write(self.ptr as *mut U, Default::default()); } 
            },
            MetadataState::Mismatch => {
                // drop the mutex guard so we don't poison it
                let msg = format!("Type mismatch for offset: {}, found: {}", offset, md.type_info[0].name);
                drop(guard);
                panic!("{}", msg);
            }
        }
        let ptr = Ptr { ptr: self.ptr as *const U };
        std::mem::forget(self);
        return ptr;
    }
}
impl<T: 'static + TypeDesc + Default> Ptr<T> {
    pub fn from_ref(ptr: &T) -> Ptr<T> {
        Self::from_void(ptr as *const T as *const c_void)
    }
    pub fn from_void(ptr: *const c_void) -> Ptr<T> {
        let ptr = ptr as *const T;
        dbg!(std::any::type_name::<T>());
        let mut guard = METADATA_STORE.data.lock().unwrap();
        dbg!(ptr as *const _);
        let Some((base, md)) = METADATA_STORE.get_mut(ptr as *const _, &mut guard) else {
            drop(guard);
            panic!("missing metadata for {:?}", ptr);
        };
        let offset = ptr as *const _ as usize - base as usize;
        md.inc_ref();
        if offset == md.size {
            // this is a pointer to the end of the object
            // we don't want to intialize it
            return Ptr { ptr }
        }
        assert!(!md.type_info.is_empty());
        match md.matches_type::<T>(offset) {
            MetadataState::Matches => {},
            MetadataState::Uninit => {
                unsafe { std::ptr::write(ptr as *mut T, Default::default()); } 
            },
            MetadataState::Mismatch => {
                // drop the mutex guard so we don't poison it
                let msg = format!("Type mismatch for offset: {}, found: {}", offset, md.type_info[0].name);
                drop(guard);
                panic!("{}", msg);
            }
        }
        return Ptr { ptr }
    }

    pub fn offset_bytes_and_cast<U: 'static + TypeDesc + Default>(self) -> Ptr<U> {
        panic!();
    }

    pub fn offset(self, count: isize) -> Ptr<T> {
        if self.is_null() {
            panic!("offset of null pointer");
        }
        // we could avoid ref count inc/dec if we specialized this
        Ptr::from_void(self.ptr.wrapping_offset(count) as *const c_void)
    }

    pub fn inc(&mut self) {
        if self.is_null() {
            panic!("offset of null pointer");
        }
        // we could avoid ref count inc/dec if we specialized this
        *self = Ptr::from_void(self.ptr.wrapping_offset(1) as *const c_void)
    }

}

impl<T: 'static + TypeDesc + Default> Ptr<T> {
    pub fn new(value: T) -> Ptr<T> {
        let ptr = malloc(std::mem::size_of::<T>()).cast();
        unsafe { std::ptr::write(ptr.ptr as *mut T, value); }
        ptr
    }
}

unsafe impl<T: Sync> Sync for Ptr<T> { }
unsafe impl<T: Send> Send for Ptr<T> { }

impl Ptr<Char> {
    pub fn new_string(str: &str) -> Self {
        let mut ptr: Ptr<Char> = malloc(str.len() + 1).cast();
        let start = ptr.clone();
        for (i, c) in str.bytes().enumerate() {
            ptr.set(c as i8);
            dbg!(i);
            ptr = ptr.offset(1);
        }
        ptr.set(0);
        start
    }
}


// makes a Ptr from a string literal
#[macro_export]
macro_rules! s {
    ($s:literal) => {
        {
            use std::sync::LazyLock;
            static _P: LazyLock<$crate::PtrCell<Char>> = LazyLock::new(|| $crate::PtrCell::new($crate::Ptr::<Char>::new_string($s)));
            _P.get()
        }
    }

}
#[test]
fn string_literal() {
    let s = s!("hello");
    assert_eq!(strlen(s), 5)
}

impl<T: 'static + TypeDesc + Default> Sub<usize> for Ptr<T> {
    type Output = Ptr<T>;

    fn sub(self, rhs: usize) -> Self::Output {
        Ptr::from_void(self.ptr.wrapping_offset(-(rhs as isize)) as *const c_void)
    }
}

// An array of these describes the layout of a type
#[derive(Debug)]
pub struct TypeInfo {
    pub offset: usize,
    pub size: usize,
    pub ty: std::any::TypeId,
    pub drop_ptr: unsafe fn(*const c_void),
    pub set_ptr: unsafe fn(*const c_void, *const c_void),
    pub name: &'static str
}

// The returned Vec must be sorted by offset
// unions can be represented by having multiple entries with the same offset
pub trait TypeDesc {
    fn type_desc() -> Vec<TypeInfo>;
}

impl TypeDesc for c_void {
    fn type_desc() -> Vec<TypeInfo> {
        return Vec::new();
    }
}

fn empty_drop(_: *const c_void) {

}

pub unsafe fn panic_set(dst: *const c_void, src: *const c_void) {
    panic!();
}

unsafe fn set_ptr<T: Set>(dst: *const c_void, src: *const c_void) {
    let src = (src as *const T).as_ref().unwrap();
    let dst = (dst as *const T).as_ref().unwrap();
    src.set(dst);
}

// macro to implement TypeDesc
macro_rules! impl_type_desc {
    ($($t:ty),*) => {
        $(
            impl TypeDesc for $t {
                fn type_desc() -> Vec<TypeInfo> {
                    vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: empty_drop, set_ptr: panic_set, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()}]
                }
            }
        )*
    };
}

impl_type_desc!(i8, i32, u32, f32);


pub unsafe fn drop_ptr_in_place<T>(ptr: *const c_void) {
    unsafe { std::ptr::drop_in_place(ptr as *mut T) };
}

impl<T: 'static> TypeDesc for Ptr<T> {
    fn type_desc() -> Vec<TypeInfo> {
        let desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: drop_ptr_in_place::<Self>, set_ptr: panic_set, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()}];
        desc
    }
}


impl TypeDesc for Foo {
    fn type_desc() -> Vec<TypeInfo> {
        let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: drop_ptr_in_place::<Self>, set_ptr: set_foo, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()}];
        for x in Cell::<i32>::type_desc() {
            desc.push(TypeInfo{ offset: offset_of!(Self, x) + x.offset, ty: x.ty, drop_ptr: x.drop_ptr, set_ptr: x.set_ptr, size: x.size, name: x.name})
        }
        for y in Cell::<i32>::type_desc() {
            desc.push(TypeInfo{ offset: offset_of!(Self, y) + y.offset, ty: y.ty, drop_ptr: y.drop_ptr, set_ptr: y.set_ptr, size: y.size, name: y.name})
        }
        desc
    }
}

unsafe fn set_foo(dst: *const c_void, src: *const c_void) {
    let src = (src as *const Foo).as_ref().unwrap();
    let dst = (dst as *const Foo).as_ref().unwrap();
    dst.x.set(src.x.get());
    dst.y.set(src.y.get());
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct SyncPtr(*const c_void);

unsafe impl Sync for SyncPtr {}
unsafe impl Send for SyncPtr {}
type PtrMap = BTreeMap<SyncPtr, Box<Metadata>>;

struct MetadataStore {
    data: Lazy<Mutex<PtrMap>>
}


impl Drop for MetadataStore {
    fn drop(&mut self) {
        panic!();
    }
}

impl MetadataStore {
    fn put(&self, addr: *const c_void, metadata: Box<Metadata>) {
        self.data.lock().unwrap().insert(SyncPtr(addr), metadata);
    }
    fn get_mut<'a>(&self, addr: *const c_void, guard: &'a mut MutexGuard<PtrMap>) -> Option<(*const c_void, &'a mut Box<Metadata>)> {
        let x = guard.range_mut(..=SyncPtr(addr)).last();
        if let Some((base, md)) = x {
            let offset = addr as usize - base.0 as usize;
            if offset > md.size {
                return None
            }
            return Some((base.0, md))
        }
        None
    }
    fn get<'a>(&self, addr: *const c_void, guard: &'a MutexGuard<PtrMap>) -> Option<(*const c_void, &'a Box<Metadata>)> {
        let x = guard.range(..=SyncPtr(addr)).last();
        if let Some((base, md)) = x {
            let offset = addr as usize - base.0 as usize;
            if offset > md.size {
                return None
            }
            return Some((base.0, md))
        }
        None
    }
    fn remove(&self, addr: *const c_void, guard: &mut MutexGuard<PtrMap>) {
        guard.remove(&SyncPtr(addr)).unwrap(); 
    }
}
static METADATA_STORE: MetadataStore = MetadataStore { data: Lazy::new(|| Mutex::new(BTreeMap::new())) };
const MAX_ALIGN: usize = 8;

pub fn malloc(size: usize) -> Ptr<c_void> {
    let ptr = Ptr { ptr: unsafe { std::alloc::alloc(Layout::from_size_align(size, MAX_ALIGN).unwrap()) as *const _ as *const _ }};
    let metadata = Box::new(Metadata { size: size, cnt: Cell::new(1), valid: true, type_info: Vec::new() });
    METADATA_STORE.put(ptr.ptr, metadata);
    ptr
}


pub fn calloc(size: usize, count: usize) -> Ptr<c_void> {
    let ptr = Ptr { ptr: unsafe { std::alloc::alloc(Layout::from_size_align(size * count, MAX_ALIGN).unwrap()) as *const _ as *const _ }};
    let metadata = Box::new(Metadata { size: size, cnt: Cell::new(1), valid: true, type_info: Vec::new() });
    METADATA_STORE.put(ptr.ptr, metadata);
    ptr
}

pub trait Reset {
    fn reset(&self);
}

impl<T: Reset> Reset for PtrCell<T> {
    fn reset(&self) {
        self.value.set(Ptr::null());
    }
}

impl<T: Reset> Reset for RcCell<T> {
    fn reset(&self) {
        self.set(Default::default());
    }
}

pub trait Set {
    fn set(&self, src: &Self);
}

pub fn memset<T: Reset + TypeDesc + 'static + Default> (ptr: Ptr<T>, value: c_int, size: usize) {
    assert_eq!(value, 0);
    if size == std::mem::size_of::<T>() {
        panic!();
    } else if size % std::mem::size_of::<T>() == 0 {
        let ptr = ptr;
        for i in 0..size / std::mem::size_of::<T>() {
            ptr[i as isize].reset();
        }
    } else {
        panic!();
    }
}

// This does a type safe memcpy
pub fn memcpy(mut dest: Ptr<c_void>, mut src: Ptr<c_void>, size: usize) {
    let mut guard = METADATA_STORE.data.lock().unwrap();
    let (dest_base, dest_md) = METADATA_STORE.get(dest.ptr as *const _, &guard).unwrap();
    let (src_base, src_md) = METADATA_STORE.get(src.ptr as *const _, &guard).unwrap();

    assert_eq!(dest.ptr, dest_base);
    //let (src_base, src_md) = METADATA_STORE.get(src.ptr as *const _, &mut guard).unwrap();
    assert_eq!(src.ptr, src_base);
    let mut i = 0;
    let mut remaining_size = size;
    while remaining_size > 0 && dest_md.type_info[i].ty == src_md.type_info[i].ty && dest_md.type_info[i].size <= remaining_size {
        unsafe { (dest_md.type_info[0].set_ptr)(dest.ptr, src.ptr); }
        remaining_size -= dest_md.type_info[i].size;
        dest.ptr = dest.ptr.wrapping_offset(dest_md.type_info[i].size as isize);
        src.ptr = src.ptr.wrapping_offset(src_md.type_info[i].size as isize);
        i += 1;
    }
    if remaining_size > 0 {
        panic!("type or size mismatch {} {}", remaining_size, i);
    }
}

pub fn memcmp(p1: Ptr<c_void>, p2: Ptr<c_void>, size: usize) -> i32 {
    let p1: Ptr<Char> = p1.cast();
    let p2: Ptr<Char> = p2.cast();
    for i in 0..size {
        let byte1 = p1.clone().offset(i as isize).get();
        let byte2 = p2.clone().offset(i as isize).get();
        if byte1 != byte2 {
            return (byte1).cmp(&(byte2)) as i32;
        }
    }
    return 0;
}



pub fn free<T>(addr: Ptr<T>) {
    eprintln!("free({:?}: Ptr<{}>)", addr.ptr, std::any::type_name::<T>());
    if addr.is_null() {
        return;
    }
    let mut guard = METADATA_STORE.data.lock().unwrap();
    let (base, md) = METADATA_STORE.get_mut(addr.ptr as *const _, &mut guard).unwrap();
    let was_valid = md.valid;
    md.valid = false;
    eprintln!("done freeing");
    let drop_ptr = md.type_info[0].drop_ptr;
    drop(guard);
    if was_valid {
        unsafe { drop_ptr(addr.ptr as *const c_void); }
    }
    assert!(was_valid, "this memory has already been freed");
    assert_eq!(base, addr.ptr as *const c_void);
}

impl<T: Default + Clone> Set for Cell<T> {
    fn set(&self, src: &Self) {
        let t = src.take();
        let t2 = t.clone();
        src.set(t);
        self.set(t2);
    }
}

impl<T: 'static + Default + Clone> TypeDesc for Cell<T> {
    fn type_desc() -> Vec<TypeInfo> {
        dbg!(std::any::type_name::<Self>());
        let target_type = std::any::TypeId::of::<Self>();
        dbg!(target_type);
        vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: drop_ptr_in_place::<Self>, set_ptr: set_ptr::<Self>, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()},
        ]
    }
}

#[test]
fn basic() {
    let r: Ptr<Foo> = malloc(std::mem::size_of::<Foo>()).cast();
    r.x.set(5);
    let m: Ptr<Cell<i32>> = r.clone().cast();
    m.set(4);

    assert_eq!(r.y.get(), 0);

    let s: Ptr<Cell<i32>> = Ptr::from_ref(&r.y);
    s.set(3);

    assert_eq!(r.x.get(), 4);
    assert_eq!(r.y.get(), 3);
    free(r);
}
#[test]
fn uaf_basic() {
    let r: Ptr<Foo> = malloc(std::mem::size_of::<Foo>()).cast();
    // XXX: we want to make it so that the lifetime of p is limited to the statement
    let p = &r.x;
    let r3 = r.clone();
    free(r3);
    //let p = &r.x;
    p.set(5);
    //free(r);
}

#[test]
#[should_panic]
fn double_free() {
    let r: Ptr<Foo> = malloc(std::mem::size_of::<Foo>()).cast();
    // XXX: we want to make it so that the lifetime of p is limited to the statement
    let p = &r.x;
    let r3 = r.clone();
    free(r3);
    //let p = &r.x;
    p.set(5);
    free(r);
}


impl<T: 'static + TypeDesc + Set + Clone + Default> TypeDesc for [T; 2] {
    fn type_desc() -> Vec<TypeInfo> {
        let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: drop_ptr_in_place::<Self>, set_ptr: set_ptr::<Self>, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()}];
        for i in 0..2 {
            for x in T::type_desc() {
                desc.push(TypeInfo{ offset: i * std::mem::size_of::<T>() + x.offset, ty: x.ty, drop_ptr: x.drop_ptr, set_ptr: x.set_ptr, size: x.size, name: x.name})
            }
        }
        desc
    }
}

impl<T: Set> Set for [T; 2] {
    fn set(&self, src: &Self) {
        for i in 0..2 {
            self[i].set(&src[i]);
        }
    }
}

// A convenience wrapper around Ptr<Cell<T>>
pub struct CellPtr<T> {
    value: Ptr<Cell<T>>
}

/**
A convenience wrapper around Cell<Ptr<T>> that implements get and allows cloning
*/
pub struct PtrCell<T> {
    value: Cell<Ptr<T>>
}

static GLOBAL_LOCK: Mutex<()> = Mutex::new(());


impl<T> PtrCell<T> {

    pub const fn new(value: Ptr<T>) -> Self {
        Self { value: Cell::new(value) }
    }
    pub fn set(&self, value: Ptr<T>) {
        let guard = GLOBAL_LOCK.lock().unwrap();
        self.value.set(value);
        drop(guard);
    }

    pub fn get(&self) -> Ptr<T> {
        let guard = GLOBAL_LOCK.lock().unwrap();
        let t = self.value.take();
        let t2 = t.clone();
        self.value.set(t);
        drop(guard);
        t2
    }

    pub const fn null() -> Self {
        PtrCell { value: Cell::new(Ptr::null()) }
    }

    pub fn is_null(&self) -> bool {
        self.get().is_null()
    }
}

// We rely on GLOBAL_LOCK for safety here.
// That lets us ensure that PtrCell is the size of a pointer
unsafe impl<T> Sync for PtrCell<T> {}

impl<T> Default for PtrCell<T> {
    fn default() -> Self {
        PtrCell { value: Cell::new(Ptr::null()) }
    }
}

impl<T> Clone for PtrCell<T> {
    fn clone(&self) -> Self {
        let t = self.value.take();
        let t2 = t.clone();
        self.value.set(t);
        PtrCell { value: Cell::new(t2) }
    }
}

pub struct RcPtr<T> {
    value: Option<Rc<T>>
}

impl <T> RcPtr<T> {
    pub fn is_null(&self) -> bool {
        self.value.is_none()
    }
    pub fn null() -> Self {
        Self { value: None }
    }
    pub fn new(val: T) -> Self {
        Self { value: Some(Rc::new(val)) }
    }
}

impl<T> Default for RcPtr<T> {
    fn default() -> Self {
        Self { value: None }
    }
}

impl<T> Clone for RcPtr<T> {
    fn clone(&self) -> Self {
        RcPtr { value: self.value.clone() }
    }
}

impl<T> Deref for RcPtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value.as_ref().unwrap()
    }
}



#[derive(Default)]
pub struct RcCell<T> {
    value: Cell<RcPtr<T>>
}
impl<T> RcCell<T> {
    pub fn new(value: RcPtr<T>) -> Self {
        Self { value: Cell::new(value) }
    }

    pub fn set(&self, value: RcPtr<T>) {
        self.value.set(value);
    }

    pub fn get(&self) -> RcPtr<T> {
        let t = self.value.take();
        let t2 = t.clone();
        self.value.set(t);
        t2
    }

    pub fn null() -> Self {
        Self { value: Cell::new(RcPtr::null()) }
    }

    pub fn is_null(&self) -> bool {
        self.get().is_null()
    }
}


impl<T> Clone for RcCell<T> {
    fn clone(&self) -> Self {
        let t = self.value.take();
        let t2 = t.clone();
        self.value.set(t);
        Self { value: Cell::new(t2) }
    }
}

impl<T: TypeDesc + 'static> TypeDesc for RcCell<T> {
    fn type_desc() -> Vec<TypeInfo> {
        vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: drop_ptr_in_place::<Self>, set_ptr: panic_set, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()}]
    }
}

#[derive(Clone, Debug, Default)]
pub struct U32 {
    value: Cell<u32>
}

impl U32 {
    pub fn new(value: u32) -> Self {
        U32 { value: Cell::new(value) }
    }

    pub fn set(&self, value: u32) {
        self.value.set(value);
    }

    pub fn get(&self) -> u32 {
        self.value.get()
    }

    pub fn wrapping_sub(&self, value: u32) {
        self.value.set(self.value.get().wrapping_sub(value));
    }

    pub fn wrapping_add(&self, value: u32) {
        self.value.set(self.value.get().wrapping_add(value));
    }
}

impl_type_desc!(U32);


#[derive(Debug, Default)]
pub struct Char {
    value: AtomicU8
}

impl Clone for Char {
    fn clone(&self) -> Self {
        Self { value: AtomicU8::new(self.value.load(Ordering::Relaxed)) }
    }
}


impl Char {
    pub const fn new(value: std::ffi::c_char) -> Self {
        Self { value: AtomicU8::new(value as u8) }
    }

    pub fn set(&self, value: std::ffi::c_char) {
        self.value.store(value as u8, Ordering::Relaxed);
    }

    pub fn get(&self) -> std::ffi::c_char {
        self.value.load(Ordering::Relaxed) as std::ffi::c_char
    }
}


impl TypeDesc for Char {
    fn type_desc() -> Vec<TypeInfo> {
        vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: empty_drop, set_ptr: set_ptr::<Self>, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()}]
    }
}

impl Set for Char {
    fn set(&self, src: &Self) {
        self.value.store(src.value.load(Ordering::Relaxed), Ordering::Relaxed);
    }
}


#[derive(Clone, Debug, Default)]
pub struct I32 {
    value: Cell<i32>
}

impl I32 {
    pub const fn new(value: i32) -> Self {
        I32 { value: Cell::new(value) }
    }

    pub fn set(&self, value: i32) {
        self.value.set(value);
    }

    pub fn get(&self) -> i32 {
        self.value.get()
    }

    pub fn wrapping_sub(&self, value: i32) {
        self.value.set(self.value.get().wrapping_sub(value));
    }

    pub fn wrapping_add(&self, value: i32) {
        self.value.set(self.value.get().wrapping_add(value));
    }
    pub fn add_assign(&self, rhs: i32) {
        self.wrapping_add(rhs);
    }
}

impl TypeDesc for I32 {
    fn type_desc() -> Vec<TypeInfo> {
        vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: empty_drop, set_ptr: set_ptr::<Self>, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()}]
    }
}

impl PartialEq for I32 {
    fn eq(&self, other: &Self) -> bool {
        self.value.get() == other.value.get()
    }
}

impl Set for I32 {
    fn set(&self, src: &Self) {
        self.value.set(src.value.get())
    }
}

impl PartialOrd for I32 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.get().partial_cmp(&other.value.get())
    }
}

impl PartialEq<i32> for I32 {
    fn eq(&self, other: &i32) -> bool {
        self.value.get() == *other
    }
}

impl PartialOrd<i32> for I32 {
    fn partial_cmp(&self, other: &i32) -> Option<std::cmp::Ordering> {
        self.value.get().partial_cmp(&other)
    }
}

impl<T: 'static> TypeDesc for PtrCell<T> {
    fn type_desc() -> Vec<TypeInfo> {
        dbg!(std::any::type_name::<Self>());
        let target_type = std::any::TypeId::of::<Self>();
        dbg!(target_type);
        vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: drop_ptr_in_place::<Self>, set_ptr: panic_set, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()},
        ]
    }
}

#[test]
fn clone_null() {
    let x = PtrCell::<i32>::null();
    let y = x.clone();
    assert!(x.is_null());
    assert!(y.is_null());
}

#[test]
fn free_null() {
    let p: Ptr<i32> = Ptr::null();
    free(p);
}

#[test]
fn dynamic_array() {
    let p: Ptr<I32> = malloc(std::mem::size_of::<[I32; 2]>()).cast();
    p[1].set(5);
    free(p);
}

#[test]
fn free_void() {
    let k = Ptr::new(5);
    for _ in 0..10 {
        let p = Ptr::new(k.clone());
        free(p.into_void());
    }
    assert_eq!(k.count(), 1); 
    free(k);
}

#[test]
fn free_using_offset() {
    let p: Ptr<I32> = malloc(std::mem::size_of::<[I32; 2]>()).cast();
    let p2 = p.clone().offset(1);
    free(p);
    // the last reference to the memory is offset. We want to make sure that
    // we free base pointer and not the offset one
    drop(p2);
}

#[test]
fn end_ptr() {
    let start: Ptr<I32> = malloc(std::mem::size_of::<[I32; 2]>()).cast();
    let end: Ptr<I32> = start.clone().offset(2);
    drop(end);
    free(start);
}

pub struct AutoPtr<T> {
    p: Ptr<T>
}

impl<T: TypeDesc + Default + 'static > AutoPtr<T> {
    pub fn new(value: T) -> Self {
        AutoPtr { p: Ptr::new(value) }
    }

    pub fn ptr(&self) -> Ptr<T> {
        self.p.clone()
    }
}

impl<T> Drop for AutoPtr<T> {
    fn drop(&mut self) {
        free(self.p.clone());
    }
}

impl<T> Deref for AutoPtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &*self.p
    }
}

#[test]
#[should_panic]
fn end_ptr_deref() {
    let start: AutoPtr<I32> = AutoPtr { p: malloc(std::mem::size_of::<[I32; 2]>()).cast() };
    let end: Ptr<I32> = start.p.clone().offset(2);
    end.set(3);
}

#[test]
fn strlen_test() {
    let s = Ptr::new_string("hello");
    assert_eq!(libc::strlen(s.clone().cast()), 5);
    free(s);
}

#[test]
fn memcpy_test() {
    let p1: Ptr<I32> = Ptr::new(I32::new(5));
    let p2: Ptr<I32> = Ptr::new(I32::new(6));
    memcpy(p1.clone().into_void(), p2.clone().into_void(), std::mem::size_of::<I32>());
    assert_eq!(p1.get(), 5);
    free(p1);
    free(p2);
}

#[test]
fn memcpy_foo() {
    let p1: Ptr<Foo> = Ptr::new(Foo {x: Cell::new(5), y: Cell::new(6)});
    let p2: Ptr<Foo> = Ptr::new(Foo {x: Cell::new(7), y: Cell::new(8)});
    memcpy(p1.clone().into_void(), p2.clone().into_void(), std::mem::size_of::<Foo>());
    assert_eq!(p1.x.get(),7);
    assert_eq!(p1.y.get(),8);
    free(p1);
    free(p2);
}

#[test]
fn static_ptr_cell() {
    static P: PtrCell<I32> = PtrCell::new(Ptr::null());
    let p = Ptr::new(I32::new(5));
    P.set(p);
}