pub fn program() -> String {
    let program = "

        fn testfn1(a: bool) -> i32 {
            let b: i32 = (((1 + 2 + 3)));
            if a {
                let c: i32 = 1;
                return c
            } else {
                return (b)
            };
        }

        fn testfn2() -> i32 {
            {{{ return testfn1(true); }}}
        }

        fn testfn3(d: bool, e: bool) -> i32 {
            let f: bool = d && e;
            let n: i32 = 1;
            while f == true {
                n += 1;
                f = false;
            };
            return n;    
        }

        fn main() -> i32 {
            let g: i32 = testfn2() + testfn3(true, true);
            return g
        }

        "
    .to_string();

    return program;
}
