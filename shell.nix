let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = with pkgs; [ gcc llvm_11 libffi libxml2  ];
  LLVM_SYS_110_PREFIX="${pkgs.llvm_11}";
}
