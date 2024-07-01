/*
 * Linux klist like system
 *
 * Copyright (c) 2016-2017 Fabrice Bellard
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

use std::cell::Cell;

use memoffset::offset_of;

use c_ptr::{free, malloc, Ptr, PtrCell, TypeDesc, TypeInfo};
#[derive(Default, TypeDesc)]
struct list_head {
    prev: PtrCell<list_head>,
    next: PtrCell<list_head>
}

fn init_list_head(head: Ptr<list_head>) {
    head.prev.set(head.clone());
    head.next.set(head.clone());
}

fn __list_add(el: Ptr<list_head>, prev: Ptr<list_head>, next: Ptr<list_head>) {
    prev.next.set(el.clone());
    el.prev.set(prev);
    el.next.set(next.clone());
    next.prev.set(el.clone());
}

fn cell_clone<T: Default + Clone>(target: &Cell<T>) -> T {
    let temp = target.take();
    let result = temp.clone();
    target.set(temp);
    result
}

fn list_add(el: Ptr<list_head>, head: Ptr<list_head>) {
    __list_add(el, head.clone(), head.next.get());
}

fn list_add_tail(el: Ptr<list_head>, head: Ptr<list_head>) {
    __list_add(el, head.prev.get(), head.clone());
}

fn list_del(el: Ptr<list_head>) {
    let prev = el.prev.get();
    let next = el.next.get();
    prev.next.set(next.clone());
    next.prev.set(prev);
    el.prev.set(Ptr::null());
    el.next.set(Ptr::null());
}

fn list_empty(el: Ptr<list_head>) -> bool {
    Ptr::ptr_eq(&el.next.get(), &el)
}

fn iterate(head: Ptr<list_head>) {
    let mut el = head.next.get();
    while !Ptr::ptr_eq(&el, &head) {
        el = head.next.get();
    }

}

 
// impl TypeDesc for list_head {
//     fn type_desc() -> Vec<TypeInfo> {
//         let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), name: std::any::type_name::<Self>()}];
//         for prev in Ptr::<list_head>::type_desc() {
//             desc.push(TypeInfo{ offset: offset_of!(Self, prev) + prev.offset, ty: prev.ty, name: prev.name})
//         }
//         for next in Ptr::<list_head>::type_desc() {
//             desc.push(TypeInfo{ offset: offset_of!(Self, next) + next.offset, ty: next.ty, name: next.name})
//         }
//         desc
//     }
// }


//XXX: this doesn't work for non-repr C types
// impl TypeDesc for Foo {
//     fn type_desc() -> Vec<TypeInfo> {
//         let mut desc = vec![TypeInfo{ offset: 0, ty: std::any::TypeId::of::<Self>(), name: std::any::type_name::<Self>()}];
//         for x in Cell::<i32>::type_desc() {
//             desc.push(TypeInfo{ offset: offset_of!(Self, x) + x.offset, ty: x.ty, name: x.name})
//         }
//         for link in list_head::type_desc() {
//             desc.push(TypeInfo{ offset: offset_of!(Self, link) + link.offset, ty: link.ty, name: link.name})
//         }
//         desc
//     }
// }

macro_rules! list_entry {
    ($el: expr, $t: ty, $member: ident) => {
        (Ptr::<$t>::from_usize($el.value() - memoffset::offset_of!($t, $member)))
    }
}
#[derive(Default, TypeDesc)]
#[repr(C)]
struct Foo {
    x: Cell<i32>,
    link: list_head
}

pub fn do_sum() {

    let list: Ptr<list_head> = malloc(std::mem::size_of::<list_head>()).cast();
    init_list_head(list.clone());

    let mut f: Ptr<Foo> = malloc(std::mem::size_of::<Foo>()).cast();
    println!("malloc {:?}", f.value());

    f.x.set(1);
    dbg!(memoffset::offset_of!(Foo, link));
    let el = Ptr::from_ref(&f.link);
    list_add(el, list.clone()); 

    let mut f: Ptr<Foo> = malloc(std::mem::size_of::<Foo>()).cast();
    println!("malloc {:?}", f.value());
    f.x.set(5);
    list_add(Ptr::from_ref(&f.link), list.clone());

    let mut f: Ptr<Foo> = malloc(std::mem::size_of::<Foo>()).cast();
    println!("malloc {:?}", f.value());

    f.x.set(9);
    list_add(Ptr::from_ref(&f.link), list.clone());

    let result = iterate_foo(list.clone());
    assert_eq!(result, 15);
    free(list);

}


fn iterate_foo(head: Ptr<list_head>) -> i32 {
    let mut sum = 0;
    let mut el = head.next.get();
    while !Ptr::ptr_eq(&el, &head) {
        let f = list_entry!(el, Foo, link);
        sum += f.x.get();
        el = el.next.get();
    }
    sum
}


#[test]
fn list() {
    do_sum();

}