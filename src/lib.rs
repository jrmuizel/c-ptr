
use std::{alloc::Layout, cell::Cell, collections::BTreeMap, ffi::{c_int, c_void}, ops::{Deref, Index, Sub}, ptr::NonNull, sync::{Mutex, MutexGuard}};

use memoffset::offset_of;
use once_cell::sync::Lazy;
pub use c_ptr_derive::TypeDesc;



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
                self.type_info.push(TypeInfo { ty: ty.ty, drop_ptr: ty.drop_ptr, offset: ty.offset + offset, size: ty.size, name: ty.name})
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
                    desc.push(TypeInfo { ty: ty.ty, drop_ptr: ty.drop_ptr, offset: ty.offset + offset, size: ty.size, name: ty.name})
                }
                self.type_info.splice(i..i, desc);
                return MetadataState::Uninit;
            }
            eprintln!("no match");
            return MetadataState::Mismatch;
        }
    } 
}

struct Rc<T> {
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

    pub fn null() -> Ptr<T> {
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
        let (_, md) = METADATA_STORE.get(self.ptr as *const c_void, &mut guard).expect("pointer should have metadata");
        md.cnt.get()
    }
}

impl<T: Default + TypeDesc + 'static> Index<isize> for Ptr<T> {
    type Output = T;

    fn index(&self, index: isize) -> &Self::Output {
        dbg!("indexing");
        println!("indexing stdout");
        let ptr = self.ptr.wrapping_offset(index);
        dbg!(std::any::type_name::<T>());
        let mut guard = METADATA_STORE.data.lock().unwrap();
        dbg!(ptr as *const _);
        let (base, md) = METADATA_STORE.get(ptr as *const _, &mut guard).unwrap();
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

impl<T> Clone for Rc<T> {
    fn clone(&self) -> Self {
        unsafe { self.metadata.as_ref().inc_ref() };
        Rc { ptr: self.ptr, metadata: self.metadata }
    }
}

impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        if self.is_null() {
            return Self::null();
        }
        let mut guard = METADATA_STORE.data.lock().unwrap();
        let (_base, md) = METADATA_STORE.get(self.ptr as *const _, &mut guard).expect("cloning pointer without metadata");
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
        let (base, md) = METADATA_STORE.get(self.ptr as *const _, &mut guard).expect("pointer should have metadata");
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
    let (base, md) = METADATA_STORE.get(ptr as *const _, &mut guard).unwrap();
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
        let (base, md) = METADATA_STORE.get(self.ptr as *const _, &mut guard).unwrap();
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
        let (base, md) = METADATA_STORE.get(ptr as *const _, &mut guard).unwrap();
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
        // we could avoid ref count inc/dec if we specialized this
        Ptr::from_void(self.ptr.wrapping_offset(count) as *const c_void)
    }

}

impl<T: 'static + TypeDesc + Default> Ptr<T> {
    pub fn new(value: T) -> Ptr<T> {
        let ptr = malloc(std::mem::size_of::<T>()).cast();
        unsafe { std::ptr::write(ptr.ptr as *mut T, value); }
        ptr
    }
}

impl Ptr<Cell<core::ffi::c_char>> {
    pub fn new_string(str: &str) -> Self {
        let mut ptr: Ptr<Cell<core::ffi::c_char>> = malloc(str.len() + 1).cast();
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

// macro to implement TypeDesc
macro_rules! impl_type_desc {
    ($($t:ty),*) => {
        $(
            impl TypeDesc for $t {
                fn type_desc() -> Vec<TypeInfo> {
                    vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: empty_drop, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()}]
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
        let desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: drop_ptr_in_place::<Self>, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()}];
        desc
    }
}


impl TypeDesc for Foo {
    fn type_desc() -> Vec<TypeInfo> {
        let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: drop_ptr_in_place::<Self>, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()}];
        for x in Cell::<i32>::type_desc() {
            desc.push(TypeInfo{ offset: offset_of!(Self, x) + x.offset, ty: x.ty, drop_ptr: x.drop_ptr, size: x.size, name: x.name})
        }
        for y in Cell::<i32>::type_desc() {
            desc.push(TypeInfo{ offset: offset_of!(Self, y) + y.offset, ty: y.ty, drop_ptr: y.drop_ptr, size: y.size, name: y.name})
        }
        desc
    }
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
    fn get<'a>(&self, addr: *const c_void, guard: &'a mut MutexGuard<PtrMap>) -> Option<(*const c_void, &'a mut Box<Metadata>)> {
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

pub trait Reset {
    fn reset(&self);
}

impl<T: Reset> Reset for PtrCell<T> {
    fn reset(&self) {
        self.value.set(Ptr::null());
    }
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

pub fn memcpy(_dest: Ptr<c_void>, _src: Ptr<c_void>, _size: usize) {
    panic!();
}

pub fn strlen(mut str: Ptr<Cell<core::ffi::c_char>>) -> core::ffi::c_ulong {
        let mut count = 0;
        while str.get() != 0 { // Loop until null terminator is reached
          count += 1;
          str = str.offset(1);
        }
        return count;
}

pub fn free<T>(addr: Ptr<T>) {
    eprintln!("free({:?}: Ptr<{}>)", addr.ptr, std::any::type_name::<T>());
    if addr.is_null() {
        return;
    }
    let mut guard = METADATA_STORE.data.lock().unwrap();
    let (base, md) = METADATA_STORE.get(addr.ptr as *const _, &mut guard).unwrap();
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

impl<T: 'static> TypeDesc for Cell<T> {
    fn type_desc() -> Vec<TypeInfo> {
        dbg!(std::any::type_name::<Self>());
        let target_type = std::any::TypeId::of::<Self>();
        dbg!(target_type);
        vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: drop_ptr_in_place::<Self>, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()},
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




impl<T: 'static + TypeDesc> TypeDesc for [T; 2] {
    fn type_desc() -> Vec<TypeInfo> {
        let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: drop_ptr_in_place::<Self>, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()}];
        for i in 0..2 {
            for x in T::type_desc() {
                desc.push(TypeInfo{ offset: i * std::mem::size_of::<T>() + x.offset, ty: x.ty, drop_ptr: x.drop_ptr, size: x.size, name: x.name})
            }
        }
        desc
    }
}


/**
A convenience wrapper around Cell<Ptr<T>> that implements get and allows cloning
*/
pub struct PtrCell<T> {
    value: Cell<Ptr<T>>
}

impl<T> PtrCell<T> {
    pub fn set(&self, value: Ptr<T>) {
        self.value.set(value);
    }

    pub fn get(&self) -> Ptr<T> {
        let t = self.value.take();
        let t2 = t.clone();
        self.value.set(t);
        t2
    }

    pub fn null() -> Self {
        PtrCell { value: Cell::new(Ptr::null()) }
    }

    pub fn is_null(&self) -> bool {
        self.get().is_null()
    }
}

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


#[derive(Clone, Debug, Default)]
pub struct I32 {
    value: Cell<i32>
}

impl I32 {
    pub fn new(value: i32) -> Self {
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

impl_type_desc!(I32);

impl PartialEq for I32 {
    fn eq(&self, other: &Self) -> bool {
        self.value.get() == other.value.get()
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
        vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), drop_ptr: drop_ptr_in_place::<Self>, size: std::mem::size_of::<Self>(), name: std::any::type_name::<Self>()},
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

#[test]
#[should_panic]
fn end_ptr_deref() {
    struct AutoPtr {
        p: Ptr<I32>
    }
    impl Drop for AutoPtr {
        fn drop(&mut self) {
            free(self.p.clone());
        }
    }
    let start = AutoPtr { p: malloc(std::mem::size_of::<[I32; 2]>()).cast() };
    let end: Ptr<I32> = start.p.clone().offset(2);
    end.set(3);
}

#[test]
fn strlen_test() {
    let s = Ptr::new_string("hello");
    assert_eq!(strlen(s.clone().cast()), 5);
    free(s);
}