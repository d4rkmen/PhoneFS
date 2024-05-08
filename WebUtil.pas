unit WebUtil;

{------------------------------------------------------------------------------}
{  (Very) efficient Pascal implementations of :                                }
{                                                                              }
{  -URLEncode/URLDecode (Following the RFC2396 specification)                  }
{  -HTMLEncode (See: http://www.w3.org/TR/WD-html40-970708/sgml/entities.html) }
{  -XHTML named chars (See: http://www.redstart.com/XHTML/entities.htm)        }
{                                                                              }
{   Supporting the complete ISO-LATIN1 (8859-1) character set                  }
{                                                                              }
{  (c) 2001, Danny Heijl (danny.heijl@pandora.be)                              }
{  Provided as freeware. Use at your own risk. No strings attached.            }
{  (c) 2003, Zdravko Stoychev (zdravko@5group.com)                             }
{  Added XML named character apostrophe (') support                            }
{                                                                              }
{------------------------------------------------------------------------------}

{
*******************************************************************************
* Descriptions: Main Unit for FMA
* $Source: /cvsroot/fma/fma/components/WebUtil.pas,v $
* $Locker:  $
*
* Todo:
*   - add cyrillic named chars support
*
* Change Log:
* $Log: WebUtil.pas,v $
* Revision 1.6  2005/02/17 14:00:16  z_stoichev
* Fixed space encode/decode issue.
*
* Revision 1.5  2005/02/14 10:22:31  z_stoichev
* Encode Space too.
*
* Revision 1.4  2005/02/08 15:39:09  voxik
* Merged with L10N branch
*
* Revision 1.3.12.1  2005/01/07 17:57:48  expertone
* Merge with MAIN branch
*
* Revision 1.3  2004/03/26 14:27:00  z_stoichev
* Optional encoding of Non US Asci chars.
*
* Revision 1.2  2003/11/28 09:38:08  z_stoichev
* Merged with branch-release-1-1 (Fma 0.10.28c)
*
* Revision 1.1.2.1  2003/11/07 16:45:56  z_stoichev
* Initial checkin.
*
*
*
}

interface

uses SysUtils;

type
  EURLDecode = class(Exception);

{ WEB Utility functions }

function URLEncode(Src: string): string;
function URLDecode(Src: string): string;
function HTMLEncode(Src: string; EncodeNonUSASCII: boolean = True): string;
function HTMLDecode(Src: string): string;


implementation

{ URLEncode / URLDecode }

(*
 ********************
 * RFC 2396 states: *
 ********************

  alpha    = lowalpha | upalpha
  lowalpha = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" |
             "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" |
             "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
  upalpha  = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" |
             "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" |
             "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
  digit    = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" |
             "8" | "9"
  alphanum = alpha | digit
  reserved    = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" |
                "$" | ","

  unreserved  = alphanum | mark

  mark        = "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"

  escaped     = "%" hex hex
  hex         = digit | "A" | "B" | "C" | "D" | "E" | "F" |
                        "a" | "b" | "c" | "d" | "e" | "f"
UNSAFE :
  control     = <US-ASCII coded characters 00-1F and 7F hexadecimal>
  space       = <US-ASCII coded character 20 hexadecimal>
  delims      = "<" | ">" | "#" | "%" | <">
  unwise      = "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"
*)

{ URLEncode }

function URLEncode(Src: string) : string;
const
  Hex : Array[0..15] of Char = (
       '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F' // do not localize
       );
var
  i : Integer;
  Buf, P: PChar;
  ch: Char;
begin
  Result := '';
  GetMem(Buf, (Length(src) * 3) + 3);
  try
    P := Buf;
    for i := 1 to Length(src) do begin
      ch := src[i];
      if (ch in ['a'..'z']) or                                     {lowaplha}
         (ch in ['A'..'Z']) or                                     {upalpha}
         (ch in ['0'..'9']) or                                     {digit}
         (ch in ['-','_','.','!','~','*','''','(',')']) then begin {mark}
        P^ := src[i]; Inc(P);
      end else begin
        { handcoded IntToHex to avoid string handling overhead for each character }
        P^ := '%'; Inc(P);
        P^ := Hex[((Ord(ch) shr 4) and $0f)]; Inc(P);
        P^ := Hex[Ord(ch) and $0f]; Inc(P);
      end;
    end; { for }
    SetString(Result, Buf, P - Buf);
  finally
    FreeMem(Buf);
  end;
end;

{ URLDecode }

function URLDecode(src: string): string;
const
  LCHexOffset = Ord('a') - 10; // do not localize
  UCHexOffset = Ord('A') - 10; // do not localize
var
  i, l: integer;
  Buf, P: PChar;

  { convert HEX digit in ch to HEX nibble }

  function HexDigit(ch: integer): integer;
  begin
    if Char(ch) in ['0'..'9'] then begin // do not localize
      Result := ch - Ord('0');
    end else begin
      if Char(ch) in ['A'..'F'] then begin // do not localize
        Result := ch - UCHexOffset;
      end else begin
        if Char(ch) in ['a'..'f'] then begin // do not localize
          Result := ch - LCHexOffset;
        end else begin
          raise EURLDecode .Create('Invalid HEX digits in string passed to URLDecode.');
        end;
      end;
    end;
  end;

begin
  Result := '';
  GetMem(Buf, Length(src) + 2);
  try
    P := Buf;
    l := Length(src);
    i := 1;
    while i <= l do begin
      if src[i] = '%' then begin // do not localize
        if (i + 2) > l then
          raise EURLDecode.Create('Invalid URL-encoded string passed to URLDecode.');
        Inc(i);
        { handcoded HexToInt to avoid string handling overhead for each character }
        P^:= Char((HexDigit(Ord(src[i])) shl 4) or HexDigit(Ord(src[i + 1])));
        Inc(i, 2); Inc(P);
      end else begin
        P^:= src[i]; Inc(P); Inc(i);
      end;
    end;
    SetString(Result, Buf, P - Buf);
  finally
    FreeMem(Buf);
  end;
end;

{ HTML_Entities }

(*
From http://www.w3.org/TR/WD-html40-970708/sgml/entities.html :

ISO-Latin1 HTML entities :

<!-- Portions © International Organization for Standardization 1986
     Permission to copy in any form is granted for use with
     conforming SGML systems and applications as defined in
     ISO 8879, provided this notice is included in all copies.
-->

<!-- C0 Controls and Basic Latin -->
<!ENTITY quot    CDATA "&#34;"    -- quotation mark, =apl quote, u+0022 ISOnum -->
<!ENTITY amp     CDATA "&#38;"    -- ampersand, u+0026 ISOnum -->
<!ENTITY lt      CDATA "&#60;"    -- less-than sign, u+003C ISOnum -->
<!ENTITY gt      CDATA "&#62;"    -- greater-than sign, u+003E ISOnum -->

<!-- Character entity set. Typical invocation:
     <!ENTITY % HTMLlat1 PUBLIC
       "-//W3C//ENTITIES Full Latin 1//EN//HTML">
     %HTMLlat1;
-->
<!ENTITY nbsp   CDATA "&#160;" -- no-break space -->
<!ENTITY iexcl  CDATA "&#161;" -- inverted exclamation mark -->
<!ENTITY cent   CDATA "&#162;" -- cent sign -->
<!ENTITY pound  CDATA "&#163;" -- pound sterling sign -->
<!ENTITY curren CDATA "&#164;" -- general currency sign -->
<!ENTITY yen    CDATA "&#165;" -- yen sign -->
<!ENTITY brvbar CDATA "&#166;" -- broken (vertical) bar -->
<!ENTITY sect   CDATA "&#167;" -- section sign -->
<!ENTITY uml    CDATA "&#168;" -- umlaut (dieresis) -->
<!ENTITY copy   CDATA "&#169;" -- copyright sign -->
<!ENTITY ordf   CDATA "&#170;" -- ordinal indicator, feminine -->
<!ENTITY laquo  CDATA "&#171;" -- angle quotation mark, left -->
<!ENTITY not    CDATA "&#172;" -- not sign -->
<!ENTITY shy    CDATA "&#173;" -- soft hyphen -->
<!ENTITY reg    CDATA "&#174;" -- registered sign -->
<!ENTITY macr   CDATA "&#175;" -- macron -->
<!ENTITY deg    CDATA "&#176;" -- degree sign -->
<!ENTITY plusmn CDATA "&#177;" -- plus-or-minus sign -->
<!ENTITY sup2   CDATA "&#178;" -- superscript two -->
<!ENTITY sup3   CDATA "&#179;" -- superscript three -->
<!ENTITY acute  CDATA "&#180;" -- acute accent -->
<!ENTITY micro  CDATA "&#181;" -- micro sign -->
<!ENTITY para   CDATA "&#182;" -- pilcrow (paragraph sign) -->
<!ENTITY middot CDATA "&#183;" -- middle dot -->
<!ENTITY cedil  CDATA "&#184;" -- cedilla -->
<!ENTITY sup1   CDATA "&#185;" -- superscript one -->
<!ENTITY ordm   CDATA "&#186;" -- ordinal indicator, masculine -->
<!ENTITY raquo  CDATA "&#187;" -- angle quotation mark, right -->
<!ENTITY frac14 CDATA "&#188;" -- fraction one-quarter -->
<!ENTITY frac12 CDATA "&#189;" -- fraction one-half -->
<!ENTITY frac34 CDATA "&#190;" -- fraction three-quarters -->
<!ENTITY iquest CDATA "&#191;" -- inverted question mark -->
<!ENTITY Agrave CDATA "&#192;" -- capital A, grave accent -->
<!ENTITY Aacute CDATA "&#193;" -- capital A, acute accent -->
<!ENTITY Acirc  CDATA "&#194;" -- capital A, circumflex accent -->
<!ENTITY Atilde CDATA "&#195;" -- capital A, tilde -->
<!ENTITY Auml   CDATA "&#196;" -- capital A, dieresis or umlaut mark -->
<!ENTITY Aring  CDATA "&#197;" -- capital A, ring -->
<!ENTITY AElig  CDATA "&#198;" -- capital AE diphthong (ligature) -->
<!ENTITY Ccedil CDATA "&#199;" -- capital C, cedilla -->
<!ENTITY Egrave CDATA "&#200;" -- capital E, grave accent -->
<!ENTITY Eacute CDATA "&#201;" -- capital E, acute accent -->
<!ENTITY Ecirc  CDATA "&#202;" -- capital E, circumflex accent -->
<!ENTITY Euml   CDATA "&#203;" -- capital E, dieresis or umlaut mark -->
<!ENTITY Igrave CDATA "&#204;" -- capital I, grave accent -->
<!ENTITY Iacute CDATA "&#205;" -- capital I, acute accent -->
<!ENTITY Icirc  CDATA "&#206;" -- capital I, circumflex accent -->
<!ENTITY Iuml   CDATA "&#207;" -- capital I, dieresis or umlaut mark -->
<!ENTITY ETH    CDATA "&#208;" -- capital Eth, Icelandic -->
<!ENTITY Ntilde CDATA "&#209;" -- capital N, tilde -->
<!ENTITY Ograve CDATA "&#210;" -- capital O, grave accent -->
<!ENTITY Oacute CDATA "&#211;" -- capital O, acute accent -->
<!ENTITY Ocirc  CDATA "&#212;" -- capital O, circumflex accent -->
<!ENTITY Otilde CDATA "&#213;" -- capital O, tilde -->
<!ENTITY Ouml   CDATA "&#214;" -- capital O, dieresis or umlaut mark -->
<!ENTITY times  CDATA "&#215;" -- multiply sign -->
<!ENTITY Oslash CDATA "&#216;" -- capital O, slash -->
<!ENTITY Ugrave CDATA "&#217;" -- capital U, grave accent -->
<!ENTITY Uacute CDATA "&#218;" -- capital U, acute accent -->
<!ENTITY Ucirc  CDATA "&#219;" -- capital U, circumflex accent -->
<!ENTITY Uuml   CDATA "&#220;" -- capital U, dieresis or umlaut mark -->
<!ENTITY Yacute CDATA "&#221;" -- capital Y, acute accent -->
<!ENTITY THORN  CDATA "&#222;" -- capital THORN, Icelandic -->
<!ENTITY szlig  CDATA "&#223;" -- small sharp s, German (sz ligature) -->
<!ENTITY agrave CDATA "&#224;" -- small a, grave accent -->
<!ENTITY aacute CDATA "&#225;" -- small a, acute accent -->
<!ENTITY acirc  CDATA "&#226;" -- small a, circumflex accent -->
<!ENTITY atilde CDATA "&#227;" -- small a, tilde -->
<!ENTITY auml   CDATA "&#228;" -- small a, dieresis or umlaut mark -->
<!ENTITY aring  CDATA "&#229;" -- small a, ring -->
<!ENTITY aelig  CDATA "&#230;" -- small ae diphthong (ligature) -->
<!ENTITY ccedil CDATA "&#231;" -- small c, cedilla -->
<!ENTITY egrave CDATA "&#232;" -- small e, grave accent -->
<!ENTITY eacute CDATA "&#233;" -- small e, acute accent -->
<!ENTITY ecirc  CDATA "&#234;" -- small e, circumflex accent -->
<!ENTITY euml   CDATA "&#235;" -- small e, dieresis or umlaut mark -->
<!ENTITY igrave CDATA "&#236;" -- small i, grave accent -->
<!ENTITY iacute CDATA "&#237;" -- small i, acute accent -->
<!ENTITY icirc  CDATA "&#238;" -- small i, circumflex accent -->
<!ENTITY iuml   CDATA "&#239;" -- small i, dieresis or umlaut mark -->
<!ENTITY eth    CDATA "&#240;" -- small eth, Icelandic -->
<!ENTITY ntilde CDATA "&#241;" -- small n, tilde -->
<!ENTITY ograve CDATA "&#242;" -- small o, grave accent -->
<!ENTITY oacute CDATA "&#243;" -- small o, acute accent -->
<!ENTITY ocirc  CDATA "&#244;" -- small o, circumflex accent -->
<!ENTITY otilde CDATA "&#245;" -- small o, tilde -->
<!ENTITY ouml   CDATA "&#246;" -- small o, dieresis or umlaut mark -->
<!ENTITY divide CDATA "&#247;" -- divide sign -->
<!ENTITY oslash CDATA "&#248;" -- small o, slash -->
<!ENTITY ugrave CDATA "&#249;" -- small u, grave accent -->
<!ENTITY uacute CDATA "&#250;" -- small u, acute accent -->
<!ENTITY ucirc  CDATA "&#251;" -- small u, circumflex accent -->
<!ENTITY uuml   CDATA "&#252;" -- small u, dieresis or umlaut mark -->
<!ENTITY yacute CDATA "&#253;" -- small y, acute accent -->
<!ENTITY thorn  CDATA "&#254;" -- small thorn, Icelandic -->
<!ENTITY yuml   CDATA "&#255;" -- small y, dieresis or umlaut mark -->

*)

const HTML_Entities: Array[0..95] of string = (  // do not localize
    'nbsp',   // CDATA "&#160;" -- no-break space
    'iexcl',  // CDATA "&#161;" -- inverted exclamation mark
    'cent',   // CDATA "&#162;" -- cent sign
    'pound',  // CDATA "&#163;" -- pound sterling sign
    'curren', // CDATA "&#164;" -- general currency sign
    'yen',    // CDATA "&#165;" -- yen sign
    'brvbar', // CDATA "&#166;" -- broken (vertical) bar
    'sect',   // CDATA "&#167;" -- section sign
    'uml',    // CDATA "&#168;" -- umlaut (dieresis)
    'copy',   // CDATA "&#169;" -- copyright sign
    'ordf',   // CDATA "&#170;" -- ordinal indicator, feminine
    'laquo',  // CDATA "&#171;" -- angle quotation mark, left
    'not',    // CDATA "&#172;" -- not sign
    'shy',    // CDATA "&#173;" -- soft hyphen
    'reg',    // CDATA "&#174;" -- registered sign
    'macr',   // CDATA "&#175;" -- macron
    'deg',    // CDATA "&#176;" -- degree sign
    'plusmn', // CDATA "&#177;" -- plus-or-minus sign
    'sup2',   // CDATA "&#178;" -- superscript two
    'sup3',   // CDATA "&#179;" -- superscript three
    'acute',  // CDATA "&#180;" -- acute accent
    'micro',  // CDATA "&#181;" -- micro sign
    'para',   // CDATA "&#182;" -- pilcrow (paragraph sign)
    'middot', // CDATA "&#183;" -- middle dot
    'cedil',  // CDATA "&#184;" -- cedilla
    'sup1',   // CDATA "&#185;" -- superscript one
    'ordm',   // CDATA "&#186;" -- ordinal indicator, masculine
    'raquo',  // CDATA "&#187;" -- angle quotation mark, right
    'frac14', // CDATA "&#188;" -- fraction one-quarter
    'frac12', // CDATA "&#189;" -- fraction one-half
    'frac34', // CDATA "&#190;" -- fraction three-quarters
    'iquest', // CDATA "&#191;" -- inverted question mark
    'Agrave', // CDATA "&#192;" -- capital A, grave accent
    'Aacute', // CDATA "&#193;" -- capital A, acute accent
    'Acirc',  // CDATA "&#194;" -- capital A, circumflex accent
    'Atilde', // CDATA "&#195;" -- capital A, tilde
    'Auml',   // CDATA "&#196;" -- capital A, dieresis or umlaut mark
    'Aring',  // CDATA "&#197;" -- capital A, ring
    'AElig',  // CDATA "&#198;" -- capital AE diphthong (ligature)
    'Ccedil', // CDATA "&#199;" -- capital C, cedilla
    'Egrave', // CDATA "&#200;" -- capital E, grave accent
    'Eacute', // CDATA "&#201;" -- capital E, acute accent
    'Ecirc',  // CDATA "&#202;" -- capital E, circumflex accent
    'Euml',   // CDATA "&#203;" -- capital E, dieresis or umlaut mark
    'Igrave', // CDATA "&#204;" -- capital I, grave accent
    'Iacute', // CDATA "&#205;" -- capital I, acute accent
    'Icirc',  // CDATA "&#206;" -- capital I, circumflex accent
    'Iuml',   // CDATA "&#207;" -- capital I, dieresis or umlaut mark
    'ETH',    // CDATA "&#208;" -- capital Eth, Icelandic
    'Ntilde', // CDATA "&#209;" -- capital N, tilde
    'Ograve', // CDATA "&#210;" -- capital O, grave accent
    'Oacute', // CDATA "&#211;" -- capital O, acute accent
    'Ocirc',  // CDATA "&#212;" -- capital O, circumflex accent
    'Otilde', // CDATA "&#213;" -- capital O, tilde
    'Ouml',   // CDATA "&#214;" -- capital O, dieresis or umlaut mark
    'times',  // CDATA "&#215;" -- multiply sign
    'Oslash', // CDATA "&#216;" -- capital O, slash
    'Ugrave', // CDATA "&#217;" -- capital U, grave accent
    'Uacute', // CDATA "&#218;" -- capital U, acute accent
    'Ucirc',  // CDATA "&#219;" -- capital U, circumflex accent
    'Uuml',   // CDATA "&#220;" -- capital U, dieresis or umlaut mark
    'Yacute', // CDATA "&#221;" -- capital Y, acute accent
    'THORN',  // CDATA "&#222;" -- capital THORN, Icelandic
    'szlig',  // CDATA "&#223;" -- small sharp s, German (sz ligature)
    'agrave', // CDATA "&#224;" -- small a, grave accent
    'aacute', // CDATA "&#225;" -- small a, acute accent
    'acirc',  // CDATA "&#226;" -- small a, circumflex accent
    'atilde', // CDATA "&#227;" -- small a, tilde
    'auml',   // CDATA "&#228;" -- small a, dieresis or umlaut mark
    'aring',  // CDATA "&#229;" -- small a, ring
    'aelig',  // CDATA "&#230;" -- small ae diphthong (ligature)
    'ccedil', // CDATA "&#231;" -- small c, cedilla
    'egrave', // CDATA "&#232;" -- small e, grave accent
    'eacute', // CDATA "&#233;" -- small e, acute accent
    'ecirc',  // CDATA "&#234;" -- small e, circumflex accent
    'euml',   // CDATA "&#235;" -- small e, dieresis or umlaut mark
    'igrave', // CDATA "&#236;" -- small i, grave accent
    'iacute', // CDATA "&#237;" -- small i, acute accent
    'icirc',  // CDATA "&#238;" -- small i, circumflex accent
    'iuml',   // CDATA "&#239;" -- small i, dieresis or umlaut mark
    'eth',    // CDATA "&#240;" -- small eth, Icelandic
    'ntilde', // CDATA "&#241;" -- small n, tilde
    'ograve', // CDATA "&#242;" -- small o, grave accent
    'oacute', // CDATA "&#243;" -- small o, acute accent
    'ocirc',  // CDATA "&#244;" -- small o, circumflex accent
    'otilde', // CDATA "&#245;" -- small o, tilde
    'ouml',   // CDATA "&#246;" -- small o, dieresis or umlaut mark
    'divide', // CDATA "&#247;" -- divide sign
    'oslash', // CDATA "&#248;" -- small o, slash
    'ugrave', // CDATA "&#249;" -- small u, grave accent
    'uacute', // CDATA "&#250;" -- small u, acute accent
    'ucirc',  // CDATA "&#251;" -- small u, circumflex accent
    'uuml',   // CDATA "&#252;" -- small u, dieresis or umlaut mark
    'yacute', // CDATA "&#253;" -- small y, acute accent
    'thorn',  // CDATA "&#254;" -- small thorn, Icelandic
    'yuml');  // CDATA "&#255;" -- small y, dieresis or umlaut mark

function HTMLEncode(Src: string; EncodeNonUSASCII: boolean): string;
var
  i, j, k: integer;
  Buf, P: PChar;
  ch: Integer;

begin
  Result := '';
  if Length(src) = 0 then exit;
  GetMem(Buf, (Length(src) * 8) + 8); // to be on the *very* safe side
  try
    P := Buf;
    for i := 1 to Length(src) do begin
      ch := Ord(src[i]);
      case ch of
        32:            // space
          begin
            Move('%20', P^, 3);      // do not localize
            Inc(P, 3);
          end;
        34:            // quot
          begin
            Move('&quot;', P^, 6);   // do not localize
            Inc(P, 6);
          end;
        38:            // amp
          begin
            Move('&amp;', P^, 5);    // do not localize
            Inc(P, 5);
          end;
        39:            // apos
          begin
            Move('&apos;', P^, 6);   // do not localize
            Inc(P, 6);
          end;
        60:            // lt
          begin
            Move('&lt;', P^, 4);     // do not localize
            Inc(P, 4);
          end;
        62:            // gt
          begin
            Move('&gt;', P^, 4);     // do not localize
            Inc(P, 4);
          end;
        160..255:      // the NON-USASCII characters
        if EncodeNonUSASCII then
          begin
            j := ch - 160;
            P^:= '&'; Inc(P);        // do not localize
            for k := 1 to Length(HTML_Entities[j]) do begin
              P^ := HTML_Entities[j][k];
              Inc(P)
            end;
            P^:= ';'; Inc(P);
          end
        else
          begin
            P^:= Char(ch); Inc(P);
          end;
        else
          begin
            P^:= Char(ch); Inc(P);
          end;
      end;
    end;
    SetString(Result, Buf, P - Buf);
  finally
    FreeMem(Buf);
  end;
end;

{
XML 1.0 introduces a new named character reference '&apos;' to refer
to U+0027 APOSTROPHE. This entity wasn't included in any HTML version,
but it is not listed as compatibility issue in appendix C of XHTML 1.0.
When delivering XHTML documents as text/html current browsers don't
recognize this entity. There is trouble ticket #107 in the Voyager Issue
Tracking System with a note, that the HTML WG decided to add this entity
to HTML 4.01; HTML 4.01 however doesn't include this entity and it
appears to me that this decision was made after HTML 4.01 was published.
The current errata for HTML 4 doesn't list the omission of &apos;
either.

I suggest to discard the idea of adding &apos; to HTML 4.01 since it
would require to change the relevant DTDs and this is not possible in a
usable fashion through an errata; I suggest further adding another item
in appendix C of XHTML 1.0 that read e.g.

  C.xx Named Character Reference &apos;

  The named character reference &apos; (the apostrophe, U+0027) was
  introduced in XML 1.0 but didn't appear in former HTML versions.
  Authors should therefore use &#39; instead of &apos; to work as
  expected in HTML 4 user agents.
}

function HTMLDecode(Src: string): string;
var
  i: integer;
begin
  Result := '';
  i := 1;
  while i <= Length(Src) do begin
    if Copy(Src,i,3) = '%20' then begin // do not localize
      Result := Result + ' ';
      inc(i,3);
    end
    else
    if Copy(Src,i,6) = '&quot;' then begin // do not localize
      Result := Result + '"';
      inc(i,6);
    end
    else
    if Copy(Src,i,6) = '&apos;' then begin // do not localize
      Result := Result + '''';
      inc(i,6);
    end
    else
    if Copy(Src,i,5) = '&amp;' then begin
      Result := Result + '&';
      inc(i,5);
    end
    else
    if Copy(Src,i,4) = '&lt;' then begin // do not localize
      Result := Result + '<';
      inc(i,4);
    end
    else
    if Copy(Src,i,4) = '&gt;' then begin // do not localize
      Result := Result + '>';
      inc(i,4);
    end
    else begin
      Result := Result + Src[i];
      inc(i);
    end;
    // TODO: add support for // the NON-USASCII characters
  end;
end;

{
<!-- (C) International Organization for Standardization 1986
     Permission to copy in any form is granted for use with
     conforming SGML systems and applications as defined in
     ISO 8879, provided this notice is included in all copies.
-->
<!-- Character entity set. Typical invocation:
     <!ENTITY % ISOcyr1 PUBLIC
       "ISO 8879:1986//ENTITIES Russian Cyrillic//EN">
     %ISOcyr1;
-->
<!ENTITY acy    SDATA "[acy   ]"--=small a, Cyrillic-->
<!ENTITY Acy    SDATA "[Acy   ]"--=capital A, Cyrillic-->
<!ENTITY bcy    SDATA "[bcy   ]"--=small be, Cyrillic-->
<!ENTITY Bcy    SDATA "[Bcy   ]"--=capital BE, Cyrillic-->
<!ENTITY vcy    SDATA "[vcy   ]"--=small ve, Cyrillic-->
<!ENTITY Vcy    SDATA "[Vcy   ]"--=capital VE, Cyrillic-->
<!ENTITY gcy    SDATA "[gcy   ]"--=small ghe, Cyrillic-->
<!ENTITY Gcy    SDATA "[Gcy   ]"--=capital GHE, Cyrillic-->
<!ENTITY dcy    SDATA "[dcy   ]"--=small de, Cyrillic-->
<!ENTITY Dcy    SDATA "[Dcy   ]"--=capital DE, Cyrillic-->
<!ENTITY iecy   SDATA "[iecy  ]"--=small ie, Cyrillic-->
<!ENTITY IEcy   SDATA "[IEcy  ]"--=capital IE, Cyrillic-->
<!ENTITY iocy   SDATA "[iocy  ]"--=small io, Russian-->
<!ENTITY IOcy   SDATA "[IOcy  ]"--=capital IO, Russian-->
<!ENTITY zhcy   SDATA "[zhcy  ]"--=small zhe, Cyrillic-->
<!ENTITY ZHcy   SDATA "[ZHcy  ]"--=capital ZHE, Cyrillic-->
<!ENTITY zcy    SDATA "[zcy   ]"--=small ze, Cyrillic-->
<!ENTITY Zcy    SDATA "[Zcy   ]"--=capital ZE, Cyrillic-->
<!ENTITY icy    SDATA "[icy   ]"--=small i, Cyrillic-->
<!ENTITY Icy    SDATA "[Icy   ]"--=capital I, Cyrillic-->
<!ENTITY jcy    SDATA "[jcy   ]"--=small short i, Cyrillic-->
<!ENTITY Jcy    SDATA "[Jcy   ]"--=capital short I, Cyrillic-->
<!ENTITY kcy    SDATA "[kcy   ]"--=small ka, Cyrillic-->
<!ENTITY Kcy    SDATA "[Kcy   ]"--=capital KA, Cyrillic-->
<!ENTITY lcy    SDATA "[lcy   ]"--=small el, Cyrillic-->
<!ENTITY Lcy    SDATA "[Lcy   ]"--=capital EL, Cyrillic-->
<!ENTITY mcy    SDATA "[mcy   ]"--=small em, Cyrillic-->
<!ENTITY Mcy    SDATA "[Mcy   ]"--=capital EM, Cyrillic-->
<!ENTITY ncy    SDATA "[ncy   ]"--=small en, Cyrillic-->
<!ENTITY Ncy    SDATA "[Ncy   ]"--=capital EN, Cyrillic-->
<!ENTITY ocy    SDATA "[ocy   ]"--=small o, Cyrillic-->
<!ENTITY Ocy    SDATA "[Ocy   ]"--=capital O, Cyrillic-->
<!ENTITY pcy    SDATA "[pcy   ]"--=small pe, Cyrillic-->
<!ENTITY Pcy    SDATA "[Pcy   ]"--=capital PE, Cyrillic-->
<!ENTITY rcy    SDATA "[rcy   ]"--=small er, Cyrillic-->
<!ENTITY Rcy    SDATA "[Rcy   ]"--=capital ER, Cyrillic-->
<!ENTITY scy    SDATA "[scy   ]"--=small es, Cyrillic-->
<!ENTITY Scy    SDATA "[Scy   ]"--=capital ES, Cyrillic-->
<!ENTITY tcy    SDATA "[tcy   ]"--=small te, Cyrillic-->
<!ENTITY Tcy    SDATA "[Tcy   ]"--=capital TE, Cyrillic-->
<!ENTITY ucy    SDATA "[ucy   ]"--=small u, Cyrillic-->
<!ENTITY Ucy    SDATA "[Ucy   ]"--=capital U, Cyrillic-->
<!ENTITY fcy    SDATA "[fcy   ]"--=small ef, Cyrillic-->
<!ENTITY Fcy    SDATA "[Fcy   ]"--=capital EF, Cyrillic-->
<!ENTITY khcy   SDATA "[khcy  ]"--=small ha, Cyrillic-->
<!ENTITY KHcy   SDATA "[KHcy  ]"--=capital HA, Cyrillic-->
<!ENTITY tscy   SDATA "[tscy  ]"--=small tse, Cyrillic-->
<!ENTITY TScy   SDATA "[TScy  ]"--=capital TSE, Cyrillic-->
<!ENTITY chcy   SDATA "[chcy  ]"--=small che, Cyrillic-->
<!ENTITY CHcy   SDATA "[CHcy  ]"--=capital CHE, Cyrillic-->
<!ENTITY shcy   SDATA "[shcy  ]"--=small sha, Cyrillic-->
<!ENTITY SHcy   SDATA "[SHcy  ]"--=capital SHA, Cyrillic-->
<!ENTITY shchcy SDATA "[shchcy]"--=small shcha, Cyrillic-->
<!ENTITY SHCHcy SDATA "[SHCHcy]"--=capital SHCHA, Cyrillic-->
<!ENTITY hardcy SDATA "[hardcy]"--=small hard sign, Cyrillic-->
<!ENTITY HARDcy SDATA "[HARDcy]"--=capital HARD sign, Cyrillic-->
<!ENTITY ycy    SDATA "[ycy   ]"--=small yeru, Cyrillic-->
<!ENTITY Ycy    SDATA "[Ycy   ]"--=capital YERU, Cyrillic-->
<!ENTITY softcy SDATA "[softcy]"--=small soft sign, Cyrillic-->
<!ENTITY SOFTcy SDATA "[SOFTcy]"--=capital SOFT sign, Cyrillic-->
<!ENTITY ecy    SDATA "[ecy   ]"--=small e, Cyrillic-->
<!ENTITY Ecy    SDATA "[Ecy   ]"--=capital E, Cyrillic-->
<!ENTITY yucy   SDATA "[yucy  ]"--=small yu, Cyrillic-->
<!ENTITY YUcy   SDATA "[YUcy  ]"--=capital YU, Cyrillic-->
<!ENTITY yacy   SDATA "[yacy  ]"--=small ya, Cyrillic-->
<!ENTITY YAcy   SDATA "[YAcy  ]"--=capital YA, Cyrillic-->
<!ENTITY numero SDATA "[numero]"--=numero sign-->
}

{
<!-- (C) International Organization for Standardization 1986
     Permission to copy in any form is granted for use with
     conforming SGML systems and applications as defined in
     ISO 8879, provided this notice is included in all copies.
-->
<!-- Character entity set. Typical invocation:
     <!ENTITY % ISOcyr2 PUBLIC
       "ISO 8879:1986//ENTITIES Non-Russian Cyrillic//EN">
     %ISOcyr2;
-->
<!ENTITY djcy   SDATA "[djcy  ]"--=small dje, Serbian-->
<!ENTITY DJcy   SDATA "[DJcy  ]"--=capital DJE, Serbian-->
<!ENTITY gjcy   SDATA "[gjcy  ]"--=small gje, Macedonian-->
<!ENTITY GJcy   SDATA "[GJcy  ]"--=capital GJE Macedonian-->
<!ENTITY jukcy  SDATA "[jukcy ]"--=small je, Ukrainian-->
<!ENTITY Jukcy  SDATA "[Jukcy ]"--=capital JE, Ukrainian-->
<!ENTITY dscy   SDATA "[dscy  ]"--=small dse, Macedonian-->
<!ENTITY DScy   SDATA "[DScy  ]"--=capital DSE, Macedonian-->
<!ENTITY iukcy  SDATA "[iukcy ]"--=small i, Ukrainian-->
<!ENTITY Iukcy  SDATA "[Iukcy ]"--=capital I, Ukrainian-->
<!ENTITY yicy   SDATA "[yicy  ]"--=small yi, Ukrainian-->
<!ENTITY YIcy   SDATA "[YIcy  ]"--=capital YI, Ukrainian-->
<!ENTITY jsercy SDATA "[jsercy]"--=small je, Serbian-->
<!ENTITY Jsercy SDATA "[Jsercy]"--=capital JE, Serbian-->
<!ENTITY ljcy   SDATA "[ljcy  ]"--=small lje, Serbian-->
<!ENTITY LJcy   SDATA "[LJcy  ]"--=capital LJE, Serbian-->
<!ENTITY njcy   SDATA "[njcy  ]"--=small nje, Serbian-->
<!ENTITY NJcy   SDATA "[NJcy  ]"--=capital NJE, Serbian-->
<!ENTITY tshcy  SDATA "[tshcy ]"--=small tshe, Serbian-->
<!ENTITY TSHcy  SDATA "[TSHcy ]"--=capital TSHE, Serbian-->
<!ENTITY kjcy   SDATA "[kjcy  ]"--=small kje Macedonian-->
<!ENTITY KJcy   SDATA "[KJcy  ]"--=capital KJE, Macedonian-->
<!ENTITY ubrcy  SDATA "[ubrcy ]"--=small u, Byelorussian-->
<!ENTITY Ubrcy  SDATA "[Ubrcy ]"--=capital U, Byelorussian-->
<!ENTITY dzcy   SDATA "[dzcy  ]"--=small dze, Serbian-->
<!ENTITY DZcy   SDATA "[DZcy  ]"--=capital dze, Serbian-->
}

end.
