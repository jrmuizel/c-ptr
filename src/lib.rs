
use std::{alloc::Layout, cell::Cell, collections::BTreeMap, ffi::{c_int, c_void}, ops::{AddAssign, Deref, Index, Sub}, ptr::NonNull, sync::{Mutex, MutexGuard}};

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


impl Metadata {
    fn inc_ref(&self) {
        self.cnt.set(self.cnt.get() + 1);
    }

    fn matches_type<U: 'static + TypeDesc>(&mut self, offset: usize) -> bool {
        eprintln!("trying to match {} at offset {}", std::any::type_name::<U>(), offset);
        //dbg!(std::any::type_name::<Cell<i32>>());
        let target_type = std::any::TypeId::of::<U>();
        //dbg!(&self.type_info, target_type, std::any::TypeId::of::<Cell<i32>>());
        if self.type_info.is_empty() && offset + std::mem::size_of::<U>() <= self.size {
            // the memory is currently untyped, so copy in the type desc for the
            // target type
            for ty in U::type_desc() {
                self.type_info.push(TypeInfo { ty: ty.ty, offset: ty.offset + offset, name: ty.name})
            }
            eprintln!("matches because uninit");
            return true;
        } else {
            let mut last_offset = 0;
            for ty in &self.type_info {
                assert!(last_offset <= ty.offset);
                last_offset = ty.offset;
                dbg!(ty, offset);
                if ty.offset < offset {
                    continue
                }
                if ty.offset == offset {
                    if target_type == ty.ty {
                        eprintln!("found match");
                        return true;
                    }
                    continue
                }
                return false;
            }
            return false;
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

    pub fn value(&self) -> usize {
        self.ptr as usize
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
}

impl<T> Index<isize> for Ptr<T> {
    type Output = T;

    fn index(&self, index: isize) -> &Self::Output {
        let ptr = self.ptr.wrapping_offset(index);
        unsafe { raw_deref(ptr) }
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
        let (base, md) = METADATA_STORE.get(self.ptr as usize, &mut guard).expect("cloning pointer without metadata");
        md.inc_ref();
        Self { ptr: self.ptr }
    }
}

impl<T> Drop for Ptr<T> {
    fn drop(&mut self) {
        if self.ptr == std::ptr::null() {
            return;
        }
        let mut guard = METADATA_STORE.data.lock().unwrap();
        dbg!(self.ptr);
        let (base, md) = METADATA_STORE.get(self.ptr as usize, &mut guard).expect("pointer should have metadata");
        md.cnt.set(md.cnt.get() - 1);
        if md.cnt.get() == 0 {
            let valid = md.valid;
            unsafe { std::alloc::dealloc(self.ptr as *mut u8, Layout::from_size_align(md.size, MAX_ALIGN).unwrap()) }
            METADATA_STORE.remove(self.ptr as usize, &mut guard);
            assert!(!valid, "contents would've leaked");
        }
    }
}

// This is likely not safe because of the generic liftetime
unsafe fn raw_deref<'a, T>(ptr: *const T) -> &'a T {
    assert_ne!(ptr, std::ptr::null());
    let mut md = METADATA_STORE.data.lock().unwrap();
    let (base, md) = METADATA_STORE.get(ptr as usize, &mut md).unwrap();
    if !md.valid {
        panic!();
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
        let (base, md) = METADATA_STORE.get(self.ptr as usize, &mut guard).unwrap();
        let offset = self.ptr as usize - base;
        if md.type_info.is_empty() && offset + std::mem::size_of::<U>() <= md.size {
            // the memory is currently untyped, so copy in the type desc for the
            // target type
            for ty in U::type_desc() {
                md.type_info.push(TypeInfo { ty: ty.ty, offset: ty.offset + offset, name: ty.name})
            }
            unsafe { std::ptr::write(self.ptr as *mut U, Default::default()); } 
        } else if md.matches_type::<U>(offset) {
        } else {
            // drop the mutex guard so we don't poison it
            drop(guard);
            panic!("type mismatch")
        }
        let ptr = Ptr { ptr: self.ptr as *const U };
        std::mem::forget(self);
        return ptr;
    }
    pub fn from_ref(ptr: &T) -> Ptr<T> {
        dbg!(std::any::type_name::<T>());
        let mut md = METADATA_STORE.data.lock().unwrap();
        dbg!(ptr as *const _);
        let (base, md) = METADATA_STORE.get(ptr as *const _ as usize, &mut md).unwrap();
        let offset = ptr as *const _ as usize - base;
        md.inc_ref();
        assert!(!md.type_info.is_empty());
        if md.matches_type::<T>(offset) {
            return Ptr { ptr }
        }
        panic!()
    }
    pub fn from_usize(ptr: usize) -> Ptr<T> {
        let ptr = ptr as *const T;
        dbg!(std::any::type_name::<T>());
        let mut md = METADATA_STORE.data.lock().unwrap();
        dbg!(ptr as *const _);
        let (base, md) = METADATA_STORE.get(ptr as *const _ as usize, &mut md).unwrap();
        let offset = ptr as *const _ as usize - base;
        md.inc_ref();
        assert!(!md.type_info.is_empty());
        if md.matches_type::<T>(offset) {
            return Ptr { ptr }
        }
        panic!()
    }

    pub fn offset_bytes_and_cast<U: 'static + TypeDesc + Default>(self) -> Ptr<U> {
        panic!();
    }

    pub fn offset(self, count: isize) -> Ptr<T> {
        Ptr::from_usize(self.ptr.wrapping_offset(count) as usize)
    }

}

impl<T: 'static + TypeDesc + Default> Ptr<T> {
    pub fn new(value: T) -> Ptr<T> {
        let ptr = malloc(std::mem::size_of::<T>()).cast();
        unsafe { std::ptr::write(ptr.ptr as *mut T, value); }
        ptr
    }
}

impl<T: 'static + TypeDesc> Sub<usize> for Ptr<T> {
    type Output = Ptr<T>;

    fn sub(self, rhs: usize) -> Self::Output {
        Ptr::from_usize(self.ptr.wrapping_offset(-(rhs as isize)) as usize)
    }
}

// An array of these describes the layout of a type
#[derive(Debug)]
pub struct TypeInfo {
    pub offset: usize,
    pub ty: std::any::TypeId,
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

// macro to implement TypeDesc
macro_rules! impl_type_desc {
    ($($t:ty),*) => {
        $(
            impl TypeDesc for $t {
                fn type_desc() -> Vec<TypeInfo> {
                    vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), name: std::any::type_name::<Self>()}]
                }
            }
        )*
    };
}

impl_type_desc!(i8);

impl TypeDesc for i32 {
    fn type_desc() -> Vec<TypeInfo> {
        let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), name: std::any::type_name::<Self>()}];
        desc
    }
}

impl TypeDesc for u32 {
    fn type_desc() -> Vec<TypeInfo> {
        let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), name: std::any::type_name::<Self>()}];
        desc
    }
}

impl TypeDesc for f32 {
    fn type_desc() -> Vec<TypeInfo> {
        let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), name: std::any::type_name::<Self>()}];
        desc
    }
}


impl<T: 'static> TypeDesc for Ptr<T> {
    fn type_desc() -> Vec<TypeInfo> {
        let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), name: std::any::type_name::<Self>()}];
        desc
    }
}

impl TypeDesc for Foo {
    fn type_desc() -> Vec<TypeInfo> {
        let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), name: std::any::type_name::<Self>()}];
        for x in Cell::<i32>::type_desc() {
            desc.push(TypeInfo{ offset: offset_of!(Self, x) + x.offset, ty: x.ty, name: x.name})
        }
        for y in Cell::<i32>::type_desc() {
            desc.push(TypeInfo{ offset: offset_of!(Self, y) + y.offset, ty: y.ty, name: y.name})
        }
        desc
    }
}

type PtrMap = BTreeMap<usize, Box<Metadata>>;

struct MetadataStore {
    data: Lazy<Mutex<PtrMap>>
}


impl Drop for MetadataStore {
    fn drop(&mut self) {
        panic!();
    }
}

impl MetadataStore {
    fn put(&self, addr: usize, metadata: Box<Metadata>) {
        self.data.lock().unwrap().insert(addr, metadata);
    }
    fn get<'a>(&self, addr: usize, guard: &'a mut MutexGuard<PtrMap>) -> Option<(usize, &'a mut Box<Metadata>)> {
        let x = guard.range_mut(..=addr).last();
        if let Some((base, md)) = x {
            let offset = addr - base;
            if offset > md.size {
                return None
            }
            return Some((*base, md))
        }
        None
    }
    fn remove(&self, addr: usize, guard: &mut MutexGuard<PtrMap>) {
        guard.remove(&addr).unwrap(); 
    }
}
static METADATA_STORE: MetadataStore = MetadataStore { data: Lazy::new(|| Mutex::new(BTreeMap::new())) };
const MAX_ALIGN: usize = 8;

pub fn malloc(size: usize) -> Ptr<c_void> {
    let ptr = Ptr { ptr: unsafe { std::alloc::alloc(Layout::from_size_align(size, MAX_ALIGN).unwrap()) as *const _ as *const _ }};
    let metadata = Box::new(Metadata { size: size, cnt: Cell::new(1), valid: true, type_info: Vec::new() });
    METADATA_STORE.put(ptr.ptr as usize, metadata);
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

pub fn memset<T: Reset + TypeDesc + 'static>(ptr: Ptr<T>, value: c_int, size: usize) {
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

pub fn memcpy(dest: Ptr<c_void>, src: Ptr<c_void>, size: usize) {
    panic!();
}

pub fn free<T>(addr: Ptr<T>) {
    if addr.is_null() {
        return;
    }
    let mut guard = METADATA_STORE.data.lock().unwrap();
    let (base, md) = METADATA_STORE.get(addr.ptr as usize, &mut guard).unwrap();
    let was_valid = md.valid;
    md.valid = false;
    drop(guard);
    assert!(was_valid, "this memory has already been freed");
    assert_eq!(base, addr.ptr as usize);
}

impl<T: 'static> TypeDesc for Cell<T> {
    fn type_desc() -> Vec<TypeInfo> {
        dbg!(std::any::type_name::<Self>());
        let target_type = std::any::TypeId::of::<Self>();
        dbg!(target_type);
        vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), name: std::any::type_name::<Self>()},
        ]
    }
}

#[test]
fn basic() {
    let id = std::any::TypeId::of::<Foo>();
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
        let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), name: std::any::type_name::<Self>()}];
        for i in 0..2 {
            for x in T::type_desc() {
                desc.push(TypeInfo{ offset: i * std::mem::size_of::<T>() + x.offset, ty: x.ty, name: x.name})
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
        vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), name: std::any::type_name::<Self>()},
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
    let p = Ptr::null();
    free(p);
}

fn main() {
    let id = std::any::TypeId::of::<Foo>();
    let r: Ptr<Foo> = malloc(std::mem::size_of::<Foo>()).cast();
    r.x.set(5);
    let m: Ptr<Cell<i32>> = r.clone().cast();
    m.set(4);
    let s: Ptr<Cell<i32>> = Ptr::from_ref(&r.y);
    s.set(3);
    println!("Hello, world! {:?} {}", r.x.get(), r.y.get());

    free(r);
}
