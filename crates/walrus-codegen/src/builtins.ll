declare void @exit(i32)

define void @builtins.exit(i32 %status) {
    call void @exit(i32 %status)
    unreachable
}

declare i32 @putchar(i32)

define {} @builtins.putchar(i32 %c) {
    %1 = call i32 @putchar(i32 %c)
    ret {} {}
}
