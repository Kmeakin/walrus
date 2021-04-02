declare void @exit(i32)

define void @builtins.exit.wrapper(i8* %env, i32 %status) {
    call void @exit(i32 %status)
    unreachable
}

declare i32 @putchar(i32)

define {} @builtins.putchar.wrapper(i8* %env, i32 %c) {
    %1 = call i32 @putchar(i32 %c)
    ret {} {}
}
