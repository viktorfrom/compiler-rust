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
            return 7
        }

        fn testfn2() -> i32 {
            {{{ return testfn1(true); }}}
        }

        fn testfn3(d: bool, e: i32) -> i32 {
            let f: bool = d && true;
            let n: i32 = e;
            if f == true {
                n += 1;
                f = false;
            };
            return n;    
        } 

        fn main() -> i32 {
            let g: i32 = testfn2();
            let h: i32 = testfn3(true, 1);
            let i: i32 = g + h;
            return i
        }

        "
    .to_string();

    return program;
}
