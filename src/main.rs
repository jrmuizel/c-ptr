
use std::{ptr::NonNull, ffi::c_void, alloc::Layout, sync::{Mutex, MutexGuard}, collections::BTreeMap, cell::{Cell}, ops::Deref};

use memoffset::offset_of;
use once_cell::sync::Lazy;


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
        dbg!(std::any::type_name::<U>());
        dbg!(std::any::type_name::<Cell<i32>>());
        let target_type = std::any::TypeId::of::<U>();
        dbg!(&self.type_info, target_type, std::any::TypeId::of::<Cell<i32>>());
        if self.type_info.is_empty() && offset + std::mem::size_of::<U>() <= self.size {
            // the memory is currently untyped, so copy in the type desc for the
            // target type
            for ty in U::type_desc() {
                self.type_info.push(TypeInfo { ty: ty.ty, offset: ty.offset + offset})
            }
            return true;
        } else {
            for ty in &self.type_info {
                dbg!(ty, offset);
                if ty.offset < offset {
                    continue
                }
                if ty.offset == offset {
                    if target_type == ty.ty {
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

impl<T> Clone for Rc<T> {
    fn clone(&self) -> Self {
        unsafe { self.metadata.as_ref().inc_ref() };
        Rc { ptr: self.ptr, metadata: self.metadata }
    }
}

impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        let mut guard = METADATA_STORE.data.lock().unwrap();
        let (base, md) = METADATA_STORE.get(self.ptr as usize, &mut guard).unwrap();
        md.inc_ref();
        Self { ptr: self.ptr }
    }
}

impl<T> Drop for Ptr<T> {
    fn drop(&mut self) {
        let mut guard = METADATA_STORE.data.lock().unwrap();
        let (base, md) = METADATA_STORE.get(self.ptr as usize, &mut guard).unwrap();
        md.cnt.set(md.cnt.get() - 1);
        if md.cnt.get() == 0 {
            assert!(!md.valid, "contents would've leaked");
            unsafe { std::alloc::dealloc(self.ptr as *mut u8, Layout::from_size_align(md.size, MAX_ALIGN).unwrap()) }
            METADATA_STORE.remove(self.ptr as usize, &mut guard);
        }
    }
}

impl<T> Deref for Ptr<T> {

    type Target = T;

    fn deref(&self) -> &Self::Target {
        let mut md = METADATA_STORE.data.lock().unwrap();
        let (base, md) = METADATA_STORE.get(self.ptr as usize, &mut md).unwrap();
        if !md.valid {
            panic!();
        }
        unsafe { &*self.ptr }
    }
}

impl<T: 'static + TypeDesc> Ptr<T> {
    fn cast<U: 'static + TypeDesc>(self) -> Ptr<U> {
        let mut md = METADATA_STORE.data.lock().unwrap();
        let (base, md) = METADATA_STORE.get(self.ptr as usize, &mut md).unwrap();
        let offset = self.ptr as usize - base;
        if md.type_info.is_empty() && offset + std::mem::size_of::<U>() <= md.size {
            // the memory is currently untyped, so copy in the type desc for the
            // target type
            for ty in U::type_desc() {
                md.type_info.push(TypeInfo { ty: ty.ty, offset: ty.offset + offset})
            }
        } else if md.matches_type::<U>(offset) {
        } else {
            drop(md);
            panic!()
        }
        let ptr = Ptr { ptr: self.ptr as *const U };
        std::mem::forget(self);
        return ptr;
    }
    fn new(ptr: &T) -> Ptr<T> {
        let mut md = METADATA_STORE.data.lock().unwrap();
        dbg!(ptr as *const _);
        let (base, md) = METADATA_STORE.get(ptr as *const _ as usize, &mut md).unwrap();
        let offset = ptr as *const _ as usize - base;
        md.inc_ref();
        if md.matches_type::<T>(offset) {
            return Ptr { ptr }
        }
        panic!()
    }
}
#[derive(Debug)]
pub struct TypeInfo {
    offset: usize,
    ty: std::any::TypeId,
}

pub trait TypeDesc {
    fn type_desc() -> Vec<TypeInfo>;
}

impl TypeDesc for c_void {
    fn type_desc() -> Vec<TypeInfo> {
        return Vec::new();
    }
}

impl TypeDesc for Foo {
    fn type_desc() -> Vec<TypeInfo> {
        let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>()}];
        for x in Cell::<i32>::type_desc() {
            desc.push(TypeInfo{ offset: offset_of!(Self, x) + x.offset, ty: x.ty})
        }
        for y in Cell::<i32>::type_desc() {
            desc.push(TypeInfo{ offset: offset_of!(Self, y) + y.offset, ty: y.ty})
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

pub fn free<T>(addr: Ptr<T>) {
    let mut md = METADATA_STORE.data.lock().unwrap();
    let (base, md) = METADATA_STORE.get(addr.ptr as usize, &mut md).unwrap();
    assert_eq!(base, addr.ptr as usize);
    md.valid = false;
}

impl<T: 'static> TypeDesc for Cell<T> {
    fn type_desc() -> Vec<TypeInfo> {
        dbg!(std::any::type_name::<Self>());
        let target_type = std::any::TypeId::of::<Self>();
        dbg!(target_type);
        vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>()},
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
    let s: Ptr<Cell<i32>> = Ptr::new(&r.y);
    s.set(3);
    
    assert_eq!(r.x.get(), 4);
    assert_eq!(r.y.get(), 3);
    free(r);


}
fn main() {
    let id = std::any::TypeId::of::<Foo>();
    let r: Ptr<Foo> = malloc(std::mem::size_of::<Foo>()).cast();
    r.x.set(5);
    let m: Ptr<Cell<i32>> = r.clone().cast();
    m.set(4);
    let s: Ptr<Cell<i32>> = Ptr::new(&r.y);
    s.set(3);
    println!("Hello, world! {:?} {}", r.x.get(), r.y.get());

    free(r);
}
