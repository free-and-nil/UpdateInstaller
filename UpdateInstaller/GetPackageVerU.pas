{$I .\switches.inc}

unit GetPackageVerU;

interface

function Get_Package_Version (const sPackageXml: String;
                              out iVersion: Integer) : Boolean;

implementation

uses SysUtils,
{$IFDEF UNICODE}
     LibXmlParserU;
{$ELSE}
     LibXmlParser;
{$ENDIF}

(* ---- *)

function Get_Package_Version (const sPackageXml: String;
                              out iVersion: Integer) : Boolean;

var
    sVersion : String;
    iLen, iStart : Integer;

begin
    Result := false;

    with TXmlParser.Create do
        try
            LoadFromFile (sPackageXml);

            while (Scan) do
                if (CurPartType = ptPI) then
                begin
                    if (CurName = 'pcdDescriptor') and
                       (Pos ('version=', CurContent) > 0) then
                    begin
                        iLen := Length (CurContent);
                        iStart := Pos ('="', CurContent) + 2;

                        if (iStart > 0) and (iStart < iLen) then
                        begin
                            sVersion := Copy (CurContent, iStart, iLen - iStart);

                            iVersion := StrToIntDef (sVersion, (-1));
                            Result := iVersion <> (-1);
                        end; { if }

                        Break;
                    end; { if }
                end; { if }

        finally
            Free;
        end; { try / finally }
end; { Get_Package_Version }

(* ---- *)

end.

