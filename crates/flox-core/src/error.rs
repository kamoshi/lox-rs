pub trait FloxError {
    fn report(&self) -> String;
    fn report_rich(&self, source: &str) -> String;
}
