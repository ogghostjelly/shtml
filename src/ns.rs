use std::collections::HashMap;

use crate::types::MalVal;

pub fn std(data: &mut HashMap<String, MalVal>) {
    math(data);
    ds(data);
}

pub fn ds(data: &mut HashMap<String, MalVal>) {
    data.insert("map".to_string(), MalVal::BuiltinFn(ds::map));
    data.insert("list".to_string(), MalVal::BuiltinFn(ds::list));
    data.insert("vec".to_string(), MalVal::BuiltinFn(ds::vec));
}

mod ds {
    use std::collections::HashMap;

    use crate::{
        env::Error,
        types::{List, MalKey, MalVal},
    };

    pub fn map(args: List) -> Result<MalVal, Error> {
        let mut args = args.into_iter();
        let mut map = HashMap::with_capacity(args.len() / 2);

        while let Some(key) = args.next() {
            let Some(value) = args.next() else {
                return Err(Error::Error(
                    "'map' expects an even number of arguments".to_string(),
                ));
            };

            let key = match MalKey::from_value(key) {
                Ok(key) => key,
                Err(key) => {
                    return Err(Error::Error(format!(
                        "'{}' cannot be a map key",
                        key.type_name()
                    )))
                }
            };

            map.insert(key, value);
        }

        Ok(MalVal::Map(map))
    }

    pub fn list(args: List) -> Result<MalVal, Error> {
        Ok(MalVal::List(args))
    }

    pub fn vec(args: List) -> Result<MalVal, Error> {
        Ok(MalVal::Vector(args.into_vec()))
    }
}

pub fn math(data: &mut HashMap<String, MalVal>) {
    data.insert("+".to_string(), MalVal::BuiltinFn(math::add));
    data.insert("-".to_string(), MalVal::BuiltinFn(math::sub));
    data.insert("*".to_string(), MalVal::BuiltinFn(math::mul));
    data.insert("/".to_string(), MalVal::BuiltinFn(math::div));
}

mod math {
    use crate::{
        env::Error,
        types::{List, MalVal},
    };

    pub fn add(args: List) -> Result<MalVal, Error> {
        reduce(args, add2)
    }

    fn add2(fst: MalVal, snd: MalVal) -> Result<MalVal, Error> {
        match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float + (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a + b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a + b)),
            (MalVal::Str(a), MalVal::Str(b)) => Ok(MalVal::Str(format!("{a}{b}"))),
            (MalVal::List(mut a), MalVal::List(mut b)) => {
                a.append(&mut b);
                Ok(MalVal::List(a))
            }
            (MalVal::List(mut a), MalVal::Vector(b)) => {
                a.append(&mut List::from_vec(b));
                Ok(MalVal::List(a))
            }
            (MalVal::Vector(mut a), MalVal::Vector(mut b)) => {
                a.append(&mut b);
                Ok(MalVal::Vector(a))
            }
            (MalVal::Vector(mut a), MalVal::List(b)) => {
                a.append(&mut b.into_vec());
                Ok(MalVal::Vector(a))
            }
            vals => Err(error("+", vals)),
        }
    }

    pub fn sub(args: List) -> Result<MalVal, Error> {
        reduce(args, |fst, snd| match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float - (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a - b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a - b)),
            vals => Err(error("-", vals)),
        })
    }

    pub fn mul(args: List) -> Result<MalVal, Error> {
        reduce(args, |fst, snd| match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float * (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a * b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a * b)),
            vals => Err(error("*", vals)),
        })
    }

    pub fn div(args: List) -> Result<MalVal, Error> {
        reduce(args, |fst, snd| match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float / (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a / b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a / b)),
            vals => Err(error("/", vals)),
        })
    }

    fn reduce(
        args: List,
        join: impl Fn(MalVal, MalVal) -> Result<MalVal, Error>,
    ) -> Result<MalVal, Error> {
        let mut iter = args.into_iter();
        let Some(mut accum) = iter.next() else {
            return Ok(MalVal::List(List::new()));
        };

        for value in iter {
            accum = join(accum, value)?;
        }

        Ok(accum)
    }

    fn error(op: &'static str, (fst, snd): (MalVal, MalVal)) -> Error {
        Error::Error(format!(
            "cannot use '{op}' on '{}' and '{}'",
            fst.type_name(),
            snd.type_name()
        ))
    }
}
