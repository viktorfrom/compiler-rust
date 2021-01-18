pub fn program() -> String {
    let program = "

        fn testfn1(a: bool) -> i32 {
            let c: i32 = (((1 + 2 + 3)));
            if a {
                let b: i32 = 1;
                return b
            } else {
                return (c)
            };
        }

        fn testfn2() -> i32 {
            {{{ return testfn1(true); }}}
        }

        fn testfn3(b: bool, c: bool) -> i32 {
            let d: bool = b && c;
            let n: i32 = 0;
            while d == true {
                n += 1;
                d = false;
            };
            return n;    
        }

        fn main() -> i32 {
            let a: i32 = testfn2(); 
            let b: i32 = testfn3(true, true);
            return a + b
        }

        "
    .to_string();

    return program;
}
