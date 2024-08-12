use super::{env::EnvRef, error::ErrorType, types::LoxType};

pub(crate) fn eq(_: EnvRef, l: LoxType, r: LoxType) -> Result<LoxType, ErrorType> {
    match (l, r) {
        (LoxType::Nil, LoxType::Nil) => Ok(LoxType::Boolean(true)),
        (l, r) => Ok(LoxType::Boolean(l == r)),
    }
}

pub(crate) fn neq(_: EnvRef, l: LoxType, r: LoxType) -> Result<LoxType, ErrorType> {
    match (l, r) {
        (LoxType::Nil, LoxType::Nil) => Ok(LoxType::Boolean(false)),
        (l, r) => Ok(LoxType::Boolean(l != r)),
    }
}

pub(crate) fn lt(_: EnvRef, l: LoxType, r: LoxType) -> Result<LoxType, ErrorType> {
    match (l, r) {
        (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Boolean(l < r)),
        _ => Err(ErrorType::TypeMismatch("Can't compare non numbers")),
    }
}

pub(crate) fn lte(_: EnvRef, l: LoxType, r: LoxType) -> Result<LoxType, ErrorType> {
    match (l, r) {
        (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Boolean(l <= r)),
        _ => Err(ErrorType::TypeMismatch("Can't compare non numbers")),
    }
}

pub(crate) fn cmp_gte(_: EnvRef, l: LoxType, r: LoxType) -> Result<LoxType, ErrorType> {
    match (l, r) {
        (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Boolean(l >= r)),
        _ => Err(ErrorType::TypeMismatch("Can't compare non numbers")),
    }
}

pub(crate) fn cmp_gt(_: EnvRef, l: LoxType, r: LoxType) -> Result<LoxType, ErrorType> {
    match (l, r) {
        (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Boolean(l > r)),
        _ => Err(ErrorType::TypeMismatch("Can't compare non numbers")),
    }
}

pub(crate) fn logic_and(_: EnvRef, l: LoxType, r: LoxType) -> Result<LoxType, ErrorType> {
    match l.is_truthy() {
        true => Ok(r),
        false => Ok(l),
    }
}

pub(crate) fn logic_or(_: EnvRef, l: LoxType, r: LoxType) -> Result<LoxType, ErrorType> {
    match l.is_truthy() {
        true => Ok(l),
        false => Ok(r),
    }
}

pub(crate) fn math_add(_: EnvRef, l: LoxType, r: LoxType) -> Result<LoxType, ErrorType> {
    match (l, r) {
        (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Number(l + r)),
        (LoxType::String(l), LoxType::String(r)) => Ok(LoxType::String(format!("{l}{r}"))),
        _ => Err(ErrorType::TypeMismatch(
            "Can only add two numbers or two strings",
        )),
    }
}

pub(crate) fn math_sub(_: EnvRef, l: LoxType, r: LoxType) -> Result<LoxType, ErrorType> {
    match (l, r) {
        (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Number(l - r)),
        _ => Err(ErrorType::TypeMismatch("Can't sub non numbers")),
    }
}

pub(crate) fn math_mul(_: EnvRef, l: LoxType, r: LoxType) -> Result<LoxType, ErrorType> {
    match (l, r) {
        (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Number(l * r)),
        _ => Err(ErrorType::TypeMismatch("Can't mul non numbers")),
    }
}

pub(crate) fn math_div(_: EnvRef, l: LoxType, r: LoxType) -> Result<LoxType, ErrorType> {
    match (l, r) {
        (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Number(l / r)),
        _ => Err(ErrorType::TypeMismatch("Can't divide non numbers")),
    }
}
