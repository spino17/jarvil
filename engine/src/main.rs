#[macro_use]
extern crate jarvil_macros;
mod ast;
mod backend;
mod cmd;
mod code;
mod codegen;
mod constants;
mod context;
mod error;
mod lexer;
mod parser;
mod reader;
mod scope;
mod server;
mod types;
mod utils;

use crate::cmd::compile::build::build;
use crate::reader::read_file;
use jarvil::backend::chunk::{Chunk, OpCode};
use jarvil::backend::object::list::ListObject;
use jarvil::backend::object::string::StringObject;
use jarvil::backend::vm::VM;
use jarvil::backend::{data::Data, object::core::Object};
use miette::{GraphicalReportHandler, GraphicalTheme, Report};
use owo_colors::Style;
use std::env::args;

fn attach_source_code(err: Report, source: String) -> Report {
    let result: miette::Result<()> = Err(err);
    match result.map_err(|error| error.with_source_code(source)).err() {
        Some(err) => return err,
        None => unreachable!("the result should always unwrap to an error"),
    }
}

fn start_compiler(args: Vec<String>) {
    let (code_vec, code_str) = read_file("/Users/bhavyabhatt/Desktop/main.jv").unwrap();
    let result = build(code_vec);
    if let Err(err) = result {
        let err = attach_source_code(err.report(), code_str);
        println!("{:?}", err)
    }
}

/*
#[derive(Clone, Debug)]
struct Nod {
    name: String,
}
impl Nod {
    fn set_name(&mut self, name: &str) {
        self.name = name.to_string();
    }
}

impl Drop for Nod {
    fn drop(&mut self) {
        println!("being dropped!");
    }
}

#[derive(Clone, Debug)]
// struct Ptr(NonNull<ManuallyDrop<Nod>>);
struct Ptr {
    ptr: NonNull<Nod>,
}

impl Ptr {
    fn new(name: &str) -> Self {
        let x = Box::new(Nod {
            name: name.to_string(),
        });
        let x_ptr = Box::into_raw(x);
        let ptr = unsafe {
            match NonNull::new(x_ptr) {
                Some(p) => p,
                None => unreachable!("x_ptr has successful allocation"),
            }
        };
        Ptr { ptr }
    }

    fn set_name(&self, name: &str) {
        unsafe {
            (&mut *self.ptr.as_ptr()).set_name(name);
        }
    }

    fn manual_drop(&self) {
        unsafe {
            Box::from_raw(self.ptr.as_ptr());
        }
    }
}
 */

fn main() {
    miette::set_hook(Box::new(|err| {
        let mut my_theme = GraphicalTheme::default();
        my_theme.styles.linum = Style::new().bright_blue();
        my_theme.styles.error = Style::new().red();
        my_theme.styles.warning = Style::new().yellow();
        my_theme.styles.advice = Style::new().yellow();
        my_theme.styles.help = Style::new().white();
        Box::new(GraphicalReportHandler::new_themed(my_theme))
    }));
    let args: Vec<String> = args().collect();
    start_compiler(args);
    let s = StringObject::new_with_bytes("bro ");
    let v = StringObject::new_with_bytes("varimas");
    let u = StringObject::new_with_bytes("bro varima");
    let mut vm = VM::new();
    let obj1 = Object::new_with_string(s.clone(), &mut vm);
    let obj2 = Object::new_with_string(v.clone(), &mut vm);
    let obj3 = Object::new_with_string(u.clone(), &mut vm);
    vm.chunk.write_constant(Data::INT(13), 1);
    vm.chunk.write_constant(Data::INT(12), 2);
    vm.chunk.write_constant(Data::OBJ(obj1), 5);
    vm.chunk.write_constant(Data::OBJ(obj2), 5);
    vm.chunk.write_byte(OpCode::OP_ADD.to_byte(), 8);
    vm.chunk.write_constant(Data::OBJ(obj3), 5);
    vm.chunk.write_byte(OpCode::OP_ADD.to_byte(), 8);
    vm.chunk.write_byte(OpCode::OP_RETURN.to_byte(), 7);
    vm.run();
    println!("{}", vm);

    let v = ListObject::new();
    v.push(Data::INT(10));
    v.push(Data::FLOAT(11.9));
    let u = v.clone();
    u.push(Data::BOOL(false));
    //println!("v: {}", v);
    //println!("u: {}", u);
    /*
    let x = Ptr::new("BHavys");
    let y = x.clone();  // clones the pointer!
    unsafe {
        println!("x: {:?}", *(x.ptr.as_ptr()));
        println!("y: {:?}", *(y.ptr.as_ptr()));
    }
    x.set_name("other_name");
    unsafe {
        println!("x: {:?}", *(x.ptr.as_ptr()));
        println!("y: {:?}", *(y.ptr.as_ptr()));
    }
    unsafe {
        println!("x: {:?}", *(x.ptr.as_ptr()));
    }
    println!("{:?}", x.ptr.as_ptr());
    println!("{:?}", y.ptr.as_ptr());
    x.manual_drop();
     */
    // y.manual_drop();
    /*
    unsafe {
        std::mem::ManuallyDrop::drop(&mut s.0);
        std::mem::ManuallyDrop::drop(&mut v.0);
    }
     */
}
