; ModuleID = 'list_length'
source_filename = "list_length"

%List = type { i8, { i32, %List* } }

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

define i32 @length(i8* %length.env, %List %length.params.0) {
length.entry:
  %l.alloca = alloca %List, align 8
  store %List %length.params.0, %List* %l.alloca, align 8
  %l = load %List, %List* %l.alloca, align 8
  br label %match.case0.test

match.case0.test:                                 ; preds = %length.entry
  %List.discriminant3 = extractvalue %List %l, 0
  %"List::Nil.cmp_discriminant" = icmp eq i8 %List.discriminant3, 0
  br i1 %"List::Nil.cmp_discriminant", label %"match.case0.List::Nil.then", label %"match.case0.List::Nil.else"

"match.case0.List::Nil.then":                     ; preds = %match.case0.test
  %List.payload4 = extractvalue %List %l, 1
  br label %"match.case0.List::Nil.end"

"match.case0.List::Nil.else":                     ; preds = %match.case0.test
  br label %"match.case0.List::Nil.end"

"match.case0.List::Nil.end":                      ; preds = %"match.case0.List::Nil.then", %"match.case0.List::Nil.else"
  %"match.case0.List::Nil.phi" = phi i1 [ true, %"match.case0.List::Nil.then" ], [ false, %"match.case0.List::Nil.else" ]
  br i1 %"match.case0.List::Nil.phi", label %match.case0.then, label %match.case1.test

match.case0.then:                                 ; preds = %"match.case0.List::Nil.end"
  %List.payload5 = extractvalue %List %l, 1
  br label %match.end

match.case1.test:                                 ; preds = %"match.case0.List::Nil.end"
  %List.discriminant = extractvalue %List %l, 0
  %"List::Cons.cmp_discriminant" = icmp eq i8 %List.discriminant, 1
  br i1 %"List::Cons.cmp_discriminant", label %"match.case1.List::Cons.then", label %"match.case1.List::Cons.else"

"match.case1.List::Cons.then":                    ; preds = %match.case1.test
  %List.payload = extractvalue %List %l, 1
  %"List::Cons.head" = extractvalue { i32, %List* } %List.payload, 0
  br label %"match.case1.List::Cons.end"

"match.case1.List::Cons.else":                    ; preds = %match.case1.test
  br label %"match.case1.List::Cons.end"

"match.case1.List::Cons.end":                     ; preds = %"match.case1.List::Cons.then", %"match.case1.List::Cons.else"
  %"match.case1.List::Cons.phi" = phi i1 [ true, %"match.case1.List::Cons.then" ], [ false, %"match.case1.List::Cons.else" ]
  br i1 %"match.case1.List::Cons.phi", label %match.case1.then, label %match.fail

match.case1.then:                                 ; preds = %"match.case1.List::Cons.end"
  %List.payload1 = extractvalue %List %l, 1
  %"List::Cons.head2" = extractvalue { i32, %List* } %List.payload1, 0
  %"List::Cons.tail" = extractvalue { i32, %List* } %List.payload1, 1
  %"List::Cons.tail.load" = load %List, %List* %"List::Cons.tail", align 8
  %tail.alloca = alloca %List, align 8
  store %List %"List::Cons.tail.load", %List* %tail.alloca, align 8
  %length.closure.alloca = alloca { i32 (i8*, %List)*, i8* }, align 8
  %length.closure.code = getelementptr inbounds { i32 (i8*, %List)*, i8* }, { i32 (i8*, %List)*, i8* }* %length.closure.alloca, i32 0, i32 0
  store i32 (i8*, %List)* @length, i32 (i8*, %List)** %length.closure.code, align 8
  %length.closure.env = getelementptr inbounds { i32 (i8*, %List)*, i8* }, { i32 (i8*, %List)*, i8* }* %length.closure.alloca, i32 0, i32 1
  store i8* null, i8** %length.closure.env, align 8
  %length = load { i32 (i8*, %List)*, i8* }, { i32 (i8*, %List)*, i8* }* %length.closure.alloca, align 8
  %closure.code = extractvalue { i32 (i8*, %List)*, i8* } %length, 0
  %closure.env = extractvalue { i32 (i8*, %List)*, i8* } %length, 1
  %tail = load %List, %List* %tail.alloca, align 8
  %call = call i32 %closure.code(i8* %closure.env, %List %tail)
  %0 = add i32 1, %call
  br label %match.end

match.fail:                                       ; preds = %"match.case1.List::Cons.end"
  unreachable

match.end:                                        ; preds = %match.case0.then, %match.case1.then
  %match.phi = phi i32 [ %0, %match.case1.then ], [ 0, %match.case0.then ]
  ret i32 %match.phi
}

define i32 @main(i8* %main.env) {
main.entry:
  %List.alloca = alloca %List, align 8
  %List.discriminant.gep = getelementptr inbounds %List, %List* %List.alloca, i32 0, i32 0
  store i8 1, i8* %List.discriminant.gep, align 1
  %List.payload.gep = getelementptr inbounds %List, %List* %List.alloca, i32 0, i32 1
  %"List::Cons.head.gep" = getelementptr inbounds { i32, %List* }, { i32, %List* }* %List.payload.gep, i32 0, i32 0
  store i32 2, i32* %"List::Cons.head.gep", align 4
  %List.alloca1 = alloca %List, align 8
  %List.discriminant.gep2 = getelementptr inbounds %List, %List* %List.alloca1, i32 0, i32 0
  store i8 1, i8* %List.discriminant.gep2, align 1
  %List.payload.gep3 = getelementptr inbounds %List, %List* %List.alloca1, i32 0, i32 1
  %"List::Cons.head.gep4" = getelementptr inbounds { i32, %List* }, { i32, %List* }* %List.payload.gep3, i32 0, i32 0
  store i32 1, i32* %"List::Cons.head.gep4", align 4
  %List.alloca5 = alloca %List, align 8
  %List.discriminant.gep6 = getelementptr inbounds %List, %List* %List.alloca5, i32 0, i32 0
  store i8 0, i8* %List.discriminant.gep6, align 1
  %List.payload.gep7 = getelementptr inbounds %List, %List* %List.alloca5, i32 0, i32 1
  %List.load = load %List, %List* %List.alloca5, align 8
  %"List::Cons.tail.gep" = getelementptr inbounds { i32, %List* }, { i32, %List* }* %List.payload.gep3, i32 0, i32 1
  %malloccall = tail call i8* @malloc(i32 ptrtoint (%List* getelementptr (%List, %List* null, i32 1) to i32))
  %"List::Cons.tail.malloc" = bitcast i8* %malloccall to %List*
  store %List %List.load, %List* %"List::Cons.tail.malloc", align 8
  store %List* %"List::Cons.tail.malloc", %List** %"List::Cons.tail.gep", align 8
  %List.load8 = load %List, %List* %List.alloca1, align 8
  %"List::Cons.tail.gep9" = getelementptr inbounds { i32, %List* }, { i32, %List* }* %List.payload.gep, i32 0, i32 1
  %malloccall10 = tail call i8* @malloc(i32 ptrtoint (%List* getelementptr (%List, %List* null, i32 1) to i32))
  %"List::Cons.tail.malloc11" = bitcast i8* %malloccall10 to %List*
  store %List %List.load8, %List* %"List::Cons.tail.malloc11", align 8
  store %List* %"List::Cons.tail.malloc11", %List** %"List::Cons.tail.gep9", align 8
  %List.load12 = load %List, %List* %List.alloca, align 8
  %l.alloca = alloca %List, align 8
  store %List %List.load12, %List* %l.alloca, align 8
  %length.closure.alloca = alloca { i32 (i8*, %List)*, i8* }, align 8
  %length.closure.code = getelementptr inbounds { i32 (i8*, %List)*, i8* }, { i32 (i8*, %List)*, i8* }* %length.closure.alloca, i32 0, i32 0
  store i32 (i8*, %List)* @length, i32 (i8*, %List)** %length.closure.code, align 8
  %length.closure.env = getelementptr inbounds { i32 (i8*, %List)*, i8* }, { i32 (i8*, %List)*, i8* }* %length.closure.alloca, i32 0, i32 1
  store i8* null, i8** %length.closure.env, align 8
  %length = load { i32 (i8*, %List)*, i8* }, { i32 (i8*, %List)*, i8* }* %length.closure.alloca, align 8
  %closure.code = extractvalue { i32 (i8*, %List)*, i8* } %length, 0
  %closure.env = extractvalue { i32 (i8*, %List)*, i8* } %length, 1
  %l = load %List, %List* %l.alloca, align 8
  %call = call i32 %closure.code(i8* %closure.env, %List %l)
  ret i32 %call
}

declare noalias i8* @malloc(i32)
