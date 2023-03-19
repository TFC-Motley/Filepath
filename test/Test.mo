import M "mo:matchers/Matchers";
import S "mo:matchers/Suite";
import T "mo:matchers/Testable";
import Option "mo:base/Option";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import Text "mo:base/Text";
import Path "../src/Path";


type Path = Path.Path;
type Elements = Path.Elements;
type Sequence = Path.Sequence;

var TestIndex : Nat = 0;

let TestLength : Nat = 4;

let EmptyPath : Path = "";

let RootPath : Path = "/";

let Basename : Path = "elements";

let ControlPath : Path = "Yabbadabbadoo";

let TestElements : Elements = ["these","are","path","elements"];

let RelativeDirPath : Path = "these/are/path";

let FunkyDirPath : Path = "/these///are/path";

let AbsoluteDirPath : Path = RootPath # RelativeDirPath;

let AbsolutePath : Path = AbsoluteDirPath # RootPath # Basename;

let RelativePath : Path = RelativeDirPath # RootPath # Basename;

let FunkyPath : Path = "//these///are/path/elements/////";

func testElements( input : Elements ) : Bool {
  Array.equal<Path>(input, TestElements, Path.equal);
};

func testSequence( input : Sequence ) : Bool {
  var idx : Nat = 0;
  let control : Sequence = TestElements.vals();
  label loopdy loop {
    if ( idx >= TestLength ) break loopdy;
    let ctrl : Path = Option.get<Path>(control.next(), ControlPath);
    let test : Path = Option.get<Path>(input.next(), ControlPath);
    if ( ctrl != test ) return false;
    idx += 1;
  };
  Option.isNull( input.next() )
};

let suite = S.suite("Filepaths", [

    S.test("Path IS Root", 
      Path.is_root( RootPath ),
      M.equals(T.bool(true))),

    S.test("Path IS NOT Root",
      Path.is_root( ControlPath ),
      M.equals(T.bool(false))),

    S.test("Input IS an absolute path",
      Path.is_absolute( AbsolutePath ),
      M.equals(T.bool(true))),

    S.test("Input IS NOT an absolute path",
      Path.is_absolute( RelativePath ),
      M.equals(T.bool(false))),

    S.test("Input IS a relative path",
      Path.is_relative( RelativePath ),
      M.equals(T.bool(true))),

    S.test("Input IS NOT a relative path",
      Path.is_relative( AbsolutePath ),
      M.equals(T.bool(false))),

    S.test("Join two paths",
      Path.join("/this/is/", "/a/joint/path/"),
      M.equals(T.text("/this/is/a/joint/path"))),

    S.test("Sequence IS equal to absolute path",
      testSequence( Path.sequence( AbsolutePath )),
      M.equals(T.bool(true))),

    S.test("Sequence IS equal to relative path",
      testSequence( Path.sequence( RelativePath )),
      M.equals(T.bool(true))),

    S.test("Sequence IS NOT equal to funky path",
      testSequence( Path.sequence( FunkyPath )),
      M.equals(T.bool(false))),

    S.test("Elements ARE equal to absolute path",
      testElements( Path.elements( AbsolutePath ).0 ),
      M.equals(T.bool(true))),

    S.test("Elements ARE equal to relative path",
      testElements( Path.elements( RelativePath).0 ),
      M.equals(T.bool(true))),

    S.test("Elements ARE NOT equal to funky path",
      testElements( Path.elements( FunkyPath ).0 ),
      M.equals(T.bool(false))),

    S.test("Absolute path depth == 4",
      Path.depth( AbsolutePath ),
      M.equals(T.nat(4))),

    S.test("Relative path depth == 4",
      Path.depth( RelativePath ),
      M.equals(T.nat(4))),

    S.test("Funky path depth == 6",
      Path.depth( FunkyPath ),
      M.equals(T.nat(6))),

    S.test("Control path depth == 1",
      Path.depth( ControlPath ),
      M.equals(T.nat(1))),
    
    S.test("Empty path depth = 0",
      Path.depth( EmptyPath ),
      M.equals(T.nat(0))),
    
    S.test("Dirname IS equal to absolute path directory",
      Path.dirname( AbsolutePath ),
      M.equals(T.text( AbsoluteDirPath ))),

    S.test("Dirname IS equal to relative path directory",
      Path.dirname( RelativePath ),
      M.equals(T.text( RelativeDirPath ))),

    S.test("Dirname IS equal to funky path directory",
      Path.dirname( FunkyPath ),
      M.equals(T.text( FunkyDirPath ))),

    S.test("Basename IS correct for absolute path",
      Path.basename( AbsolutePath ),
      M.equals(T.text( Basename ))),

    S.test("Basename IS correct for relative path",
      Path.basename( RelativePath ),
      M.equals(T.text( Basename ))),

    S.test("Basename IS correct for funky path",
      Path.basename( FunkyPath ),
      M.equals(T.text( Basename ))),
    
]);

S.run(suite);
