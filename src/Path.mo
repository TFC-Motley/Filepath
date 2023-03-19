import Int "mo:base/Int";
import Text "mo:base/Text";
import Char "mo:base/Char";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";
import Option "mo:base/Option";
import Buffer "mo:base/Buffer";
import Principal "mo:base/Principal";

module {

  public type Path = Text;

  public type Depth = Nat;

  public type Elements = [Path];

  public type Sequence = Iter.Iter<Path>;

  public let Slash : Char = '/';

  public func equal( p1 : Path, p2 : Path ) : Bool { p1 == p2 };

  public func is_root( path : Path ) : Bool { path == Text.fromChar( Slash ) };

  public func is_absolute( path : Path ) : Bool { Text.startsWith(path, #char( Slash )) };

  public func is_relative( path : Path ) : Bool { not is_absolute( path ) };

  public func join( p1 : Path, p2 : Path ) : Path {
    Text.trimEnd(p1, #char( Slash )) #
    Text.fromChar( Slash ) #
    Text.trim(p2, #char( Slash ) );
  };

  public func sequence( path : Path ) : Sequence {
    if ( is_root( path ) ) return object { public func next() : ?Path { null } };
    Text.split(Text.trim(path, #char( Slash )), #char( Slash ));
  };

  public func elements( path : Path ) : (Elements, Depth) {
    var depth : Depth = 0;
    let buffer = Buffer.Buffer<Path>(0);
    for ( elem in sequence( path ) ){
      buffer.add( elem );
      depth += 1
    };
    (Buffer.toArray( buffer ), depth)
  };

  public func index( path : Path, index : Nat ) : ?Text {
    let (elems, depth) : (Elements, Depth) = elements( path );
    if ( depth > index ) ?elems[index]
    else null;
  };

  public func depth( path : Path ) : Nat {
    var count : Nat = 0;
    for ( i in sequence( path ) ) count += 1;
    count;
  };

  public func dirname( path : Path ) : Path {
    let slash : Path = Text.fromChar( Slash );
    var ret : Path = if ( is_absolute( path ) ) slash else "";
    let (elems, depth) : (Elements, Depth) = elements( path );
    if ( depth > 1 )
      for ( i in Iter.range(0, depth-2) ){
        ret #= elems[i] # slash
      };
    Text.trimEnd(ret, #char( Slash ));
  };

  public func basename( path : Path ) : Text {
    let (elems, depth) : (Elements, Depth) = elements( path );
    if ( depth > 0 ) elems[depth-1] else "";
  };

};