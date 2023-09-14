pub trait LoxError {
    fn report(&self);

    fn wrap(err: Self) -> Box<dyn LoxError>
    where Self: Sized + 'static {
        Box::new(err)
    }
}
