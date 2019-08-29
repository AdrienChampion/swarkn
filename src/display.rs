//! A macro to concisely implement `Display`.

/// Concise notation for implementing `Display`.
///
/// # Syntax
///
/// The syntax is very similar to what you would write to actually implement `Display`, except much
/// shorter. The differences are, in order of occurrence (examples follow):
///
/// - type parameters, if any, are written between `(...)`, not `<...>`;
/// - the `std::fmt::Display` between `impl` and `for` is omitted;
/// - the `where` constraint, if any must have all its constraints between parens;
/// - the body of the `impl` is `self, <fmt_pattern> => <expr>`, where
///     - `self` binds `&self`,
///     - `<fmt_pattern>` binds the formatter,
///     - `<expr>` is the body of the `fmt` function of `Display` using the two bindings above.
///
/// # Examples
///
/// ```rust
/// use swarkn::display;
/// struct Test<'a> { content: &'a str }
/// display! {
///     impl('a) for Test<'a> {
///         self, fmt => write!(fmt, "my content is `{}`", self.content)
///     }
/// }
/// let test = Test { content: "content" };
/// assert_eq!(test.to_string(), "my content is `content`")
/// ```
///
/// ## Lifetime
///
/// ```rust
/// use swarkn::display;
/// struct Test<'a> { content: &'a str }
/// display! {
///     impl('a) for Test<'a> {
///         self, fmt => write!(fmt, "my content is `{}`", self.content)
///     }
/// }
/// let test = Test { content: "content" };
/// assert_eq!(test.to_string(), "my content is `content`")
/// ```
///
/// ## Type Parameter
///
/// ```rust
/// use swarkn::display;
/// struct Test<T> { content: T }
/// display! {
///     impl(T) for Test<T>
///     where (
///         T: std::fmt::Display,
///     ) {
///         self, fmt => write!(fmt, "my content is `{}`", self.content)
///     }
/// }
/// let test = Test { content: "content" };
/// assert_eq!(test.to_string(), "my content is `content`")
/// ```
///
/// ## Lifetime and Type Parameter
///
/// ```rust
/// use swarkn::display;
/// struct Test<'a, T> { content: &'a T }
/// display! {
///     impl('a, T) for Test<'a, T>
///     where (
///         T: std::fmt::Display + 'static,
///         'a: 'static
///     ) {
///         self, fmt => write!(fmt, "my content is `{}`", self.content)
///     }
/// }
/// let test = Test { content: &"content" };
/// assert_eq!(test.to_string(), "my content is `content`")
/// ```
#[macro_export]
macro_rules! display {
    (
        impl $( ( $($ty_arg_dec:tt)* ) )?
        for $ty:ty
        $( where ( $($constraints:tt)* ) )?
        {
            $slf:ident, $fmt:pat => $expr:expr
        }
    ) => {
        impl$(<$($ty_arg_dec)*>)? std::fmt::Display for $ty
        $(
            where $($constraints)*
        )? {
            fn fmt(&$slf, $fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                $expr
            }
        }
    };
}
