; ModuleID = 'examples/err/unnecessary_semicolon'
source_filename = "examples/err/unnecessary_semicolon"

define void @builtins.exit.wrapper(i8* %env, i32 %status) {
  call void @exit(i32 %status)
  unreachable
}

declare void @exit(i32)

define i32 @builtins.putchar.wrapper(i8* %env, i32 %c) {
  %1 = call i32 @putchar(i32 %c)
  ret i32 %1
}

declare i32 @putchar(i32)

define {} @main(i8* %main.env) {
main.entry:
  %loop.result.alloca = alloca {}, align 8
  br label %loop.body

loop.body:                                        ; preds = %loop.body, %main.entry
  br label %loop.body

loop.exit:                                        ; No predecessors!
  unreachable
}
