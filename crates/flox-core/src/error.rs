pub trait LoxError {
    fn report(&self);
    fn report_rich(&self, source: &str);
}
