use horrorshow::Render;

pub fn sanitize(s: &dyn Render) -> String {
    format!("{}", html!{ : s })
}

