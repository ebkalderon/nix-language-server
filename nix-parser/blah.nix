# hello there

{
  hello.bar.baz = 12345;

  blah = { foo = { thing  = true; };  hello = true; };

  # thing1
  #
  #   1. thing2
  #   2. hello
  broy = <hello/pkgs>;
  inherit (foo) blah;

  foo = {};
}
