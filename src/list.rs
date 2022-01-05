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

use crate::Ptr;
struct list_head {
    prev: Cell<Ptr<list_head>>,
    next: Cell<Ptr<list_head>> 
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
    __list_add(el, head.clone(), cell_clone(&head.next));
}

fn list_add_tail(el: Ptr<list_head>, head: Ptr<list_head>) {
    __list_add(el, cell_clone(&head.prev), head.clone());
}

fn list_del(el: Ptr<list_head>) {
    let prev = cell_clone(&el.prev);
    let next = cell_clone(&el.next);
    prev.next.set(next.clone());
    next.prev.set(prev);
    el.prev.take();
    el.next.take();
}

fn list_empty(el: Ptr<list_head>) -> bool {
    //el.next.get() == el
    panic!()
}