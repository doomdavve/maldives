use std::convert::TryInto;

use crate::interpreter::Interpreter;
use crate::resolvedtype::ResolvedType;
use crate::symboltable::SymbolTable;
use crate::typedexpression::TypedExpression;
use crate::typedexpressionnode::TypedExpressionNode;
use sdl2::{event::Event, keyboard::Keycode, pixels::Color};

pub fn native_println(
    _env: &mut SymbolTable,
    arguments: &[TypedExpression],
    _type_arguments: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    for argument in arguments {
        match &argument.node {
            TypedExpressionNode::String(s) => {
                println!("{}", s);
            }
            TypedExpressionNode::Integer(i) => {
                println!("{}", i);
            }
            _ => break,
        }
    }
    Ok(TypedExpression::void())
}

pub fn native_dbg(
    _env: &mut SymbolTable,
    arguments: &[TypedExpression],
    _type_arguments: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    for argument in arguments {
        println!("{:?}", argument);
    }
    Ok(TypedExpression::void())
}

pub fn native_array(
    _env: &mut SymbolTable,
    arguments: &[TypedExpression],
    type_arguments: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    match type_arguments {
        Some(types) if types.len() == 1 => Ok(TypedExpression::array(
            ResolvedType::Integer,
            arguments.to_vec(),
        )),
        _ => Err("Missing or wrong number of type arguments to array constructor".to_string()),
    }
}

pub fn native_array_len(
    _env: &mut SymbolTable,
    arguments: &[TypedExpression],
    _: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    match arguments {
        [first_arg] => match &first_arg.node {
            TypedExpressionNode::Array(array) => Ok(TypedExpression::integer(
                array.array.len().try_into().unwrap(),
            )),
            _ => Err("Missing or wrong number of arguments".to_string()),
        },
        _ => Err("Missing or wrong number of arguments".to_string()),
    }
}

pub fn native_array_map(
    env: &mut SymbolTable,
    arguments: &[TypedExpression],
    _: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    match arguments {
        [first_arg, second_arg] => match &first_arg.node {
            TypedExpressionNode::Array(array) => {
                let mut new_vec: Vec<TypedExpression> = Vec::new();
                for expr in &array.array {
                    let n = Interpreter::call(second_arg, &[expr.clone()], env)
                        .map_err(|e| e.message)?;
                    new_vec.push(n);
                }
                Ok(TypedExpression::array(
                    second_arg.resolved_type.clone(),
                    new_vec,
                ))
            }
            _ => Err("expected array".to_string()),
        },
        _ => Err("Missing or wrong number of arguments".to_string()),
    }
}

pub fn native_array_filter(
    env: &mut SymbolTable,
    arguments: &[TypedExpression],
    _: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    match arguments {
        [first_arg, second_arg] => match &first_arg.node {
            TypedExpressionNode::Array(array) => {
                let mut new_vec: Vec<TypedExpression> = Vec::new();
                for expr in &array.array {
                    let n = Interpreter::call(second_arg, &[expr.clone()], env)
                        .map_err(|e| e.message)?;
                    match &n.node {
                        TypedExpressionNode::Bool(b) => {
                            if *b {
                                new_vec.push(expr.clone())
                            }
                            Ok(())
                        }
                        _ => Err("expected bool".to_string()),
                    }?
                }
                Ok(TypedExpression::array(
                    second_arg.resolved_type.clone(),
                    new_vec,
                ))
            }
            _ => Err("expected array".to_string()),
        },
        _ => Err("Missing or wrong number of arguments".to_string()),
    }
}

pub fn native_env(
    env: &mut SymbolTable,
    _: &[TypedExpression],
    _type_arguments: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    print!("{}", env);
    Ok(TypedExpression::void())
}

/*
fn sdl() -> Result<i32, String> {
    let mut rl = Editor::<()>::new();
    let homedir = dirs::home_dir().unwrap();
    let history_file_path = Path::new(&homedir).join(".maldives_history");
    if rl.load_history(&history_file_path).is_err() {
        println!("No previous history.");
    }

    /* SDL; move to "module" */
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window("rust-sdl2 demo", 800, 600)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();

    canvas.set_draw_color(Color::RGB(0, 255, 255));
    canvas.clear();
    canvas.present();

    let mut i = 0;
    'running: loop {
        i = (i + 1) % 255;
        canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
        canvas.clear();

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }

        canvas.present();
    }

    //    loop {
    //        match rl.readline("> ") {
    //            Ok(line) if &line != "" => {
    //                rl.add_history_entry(&line);
    //                match evaluate_line(&mut root, &line) {
    //                    Ok(output) => println!("{}", output),
    //                    Err(message) => println!("{}", message),
    //                }
    //            }
    //            Ok(_) => (),
    //            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
    //                println!("Exit");
    //                break;
    //            }
    //            Err(err) => {
    //                println!("Error: {:?}", err);
    //                break;
    //            }
    //        }
    //    }
    rl.save_history(&history_file_path).unwrap();
    Ok(0)
}

*/
pub fn native_open_window(
    _env: &mut SymbolTable,
    arguments: &[TypedExpression],
    _type_arguments: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    match arguments {
        [first_arg] => match &first_arg.node {
            TypedExpressionNode::String(title) => {
                let sdl_context = sdl2::init().unwrap();
                let video_subsystem = sdl_context.video().unwrap();

                // FIXME: Add some parameters for title, size and position:
                let window = video_subsystem
                    .window(title, 800, 600)
                    .position_centered()
                    .build()
                    .unwrap();
                let mut canvas = window.into_canvas().present_vsync().build().unwrap();
                let event_pump = sdl_context.event_pump().unwrap();

                canvas.set_draw_color(Color::RGB(0, 255, 255));
                canvas.clear();
                canvas.present();

                Ok(TypedExpression::sdl(
                    sdl_context,
                    video_subsystem,
                    canvas,
                    event_pump,
                ))
            }
            _ => Ok(TypedExpression::void()), // breaks signature, throw error instead
        },
        _ => Ok(TypedExpression::void()), // breaks signature, throw error instead
    }
}

pub fn native_main_loop(
    _env: &mut SymbolTable,
    arguments: &[TypedExpression],
    _type_arguments: &Option<Vec<ResolvedType>>,
) -> Result<TypedExpression, String> {
    match arguments {
        [first_arg] => match &first_arg.node {
            TypedExpressionNode::Sdl(sdl_cell) => {
                let mut i = 0;
                'running: loop {
                    i = (i + 1) % 255;
                    let mut sdl = sdl_cell.borrow_mut();

                    sdl.canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
                    sdl.canvas.clear();
                    for event in sdl.event_pump.poll_iter() {
                        match event {
                            Event::Quit { .. }
                            | Event::KeyDown {
                                keycode: Some(Keycode::Escape),
                                ..
                            } => break 'running,
                            _ => {}
                        }
                    }

                    sdl.canvas.present();
                }

                Ok(TypedExpression::integer(0))
            }
            _ => Ok(TypedExpression::integer(0)),
        },
        _ => Ok(TypedExpression::integer(0)),
    }
}
