//
//  Original author: Olaf Hess
//  This work is published from: Germany.
//
//  To the extent possible under law, Olaf Hess has waived all copyright and
//  related or neighboring rights to this source code:
//  http://creativecommons.org/publicdomain/zero/1.0/
//
//  Unless expressly stated otherwise, the person who associated a work with
//  this deed makes no warranties about the work, and disclaims liability for
//  all uses of the work, to the fullest extent permitted by applicable law.
//

{$I ..\switches.inc}

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

unit CmdLineU;

{ The first parameter always has the index 1 in pulic methods! }

interface

uses SysUtils;

type
	EParamCheck = class (Exception);
	TCmdLineParam = class;

	TasString = array of String;
	TCheckParamEvent = procedure (const sName, sValue: String) of object;
	TValueKind = (vkNoValue, vkOptionalValue, vkValueRequired, vkNextParam);
	TValueType = (vtNone, vtInteger, vtString, vtBoolean);
	TParamUnknownAction = (puaIgnore, puaError);

	TCmdLineParam = class
	  private
		FCheckParam : TCheckParamEvent;
        FChildParam : TCmdLineParam;
        FComment : String;
        FEmptyValue : Boolean;
        FFixedPos : Integer;
        FHasFixedPos : Boolean;
		FName : String;
        FOptional : Boolean;
		FParamExists : Boolean;
		FParamIndex : Integer;
        FParentParam : TCmdLineParam;
        FParentParamName : String;
        FPrefix : Char;
        FPrefixRequired : Boolean;
        FValue : String;
		FValueKind : TValueKind;
		FValueType : TValueType;

		asMustBeUsedWith, asCannotBeUsedWith : TasString;

        function GetName : String;

	  public
		constructor Create (const sParamName: String;
							const bOptionalParam: Boolean = false;
							const bPrefixReq: Boolean = true;
                            const iFixedPos: Integer = 0;
                            const sParentParam: String = '');
        destructor Destroy; override;

		procedure CannotBeUsedWith (const List: array of String);
		procedure MustBeUsedWith (const List: array of String);
        procedure SetFixedPos (const iFixedPos: Integer);

        property ChildParam : TCmdLineParam read FChildParam;
        property Comment : String read FComment write FComment;
        property EmptyValue : Boolean read FEmptyValue;
        property FixedPos : Integer read FFixedPos;
        property Name : String read GetName;
        property HasFixedPos : Boolean read FHasFixedPos;
        property Optional : Boolean read FOptional write FOptional;
        property PrefixRequired : Boolean read FPrefixRequired;
		property ParamIndex : Integer read FParamIndex;
        property ParentParam : TCmdLineParam read FParentParam;
        property ParentParamName : String read FParentParamName;
        property Prefix : Char read FPrefix;
        property Value : String read FValue;
		property ValueKind : TValueKind read FValueKind write FValueKind
							 default vkNoValue;
		property ValueType : TValueType read FValueType write FValueType
							 default vtString;
		property OnCheckParam : TCheckParamEvent read FCheckParam
                                                 write FCheckParam;
	end; { TCmdLineParam }

	TCmdLineParams = class;

    TCmdLineParamsEnumerator = class
      private
        FIndex: Integer;
        FCmdLineParams : TCmdLineParams;

      public
        constructor Create (const CmdLineParams: TCmdLineParams);
        function GetCurrent : TCmdLineParam;
        function MoveNext : Boolean;
        property Current : TCmdLineParam read GetCurrent;
    end; { TCmdLineParamsEnumerator }

    ICmdLineParams = interface
		procedure AddParam (const Param: TCmdLineParam); overload;
        function AddParam (const sParamName: String;
        				   const bOptionalParam: Boolean;
                           const ValueKind: TValueKind = vkNoValue;
                           const ValueType: TValueType = vtNone) : TCmdLineParam;
                           											   overload;
        function AddParam (const sParamName: String;
                           const ValueKind: TValueKind = vkNoValue;
                           const ValueType: TValueType = vtNone) : TCmdLineParam;
                           											   overload;
        function AddParam (const sParamName: String;
        				   const iFixedPos: Integer;
                           const bPrefixRequired: Boolean = false;
                           const sParentParam: String = '') : TCmdLineParam;
                                                                       overload;
        function AddParam (const iFixedPos: Integer;
                           const VT: TValueType = vtString;
                           const sParentParam: String = '') : TCmdLineParam;
                                                                       overload;
        function AddParamValue (const sParentParam: String;
                                const VT: TValueType = vtString) : TCmdLineParam;
		function Count : Integer;
        function GetEnumerator : TCmdLineParamsEnumerator;
        function GetParamIndex (const sParam: String) : Integer; overload;
        function GetParamIndex (const sParam: String;
                                out iIndex: Integer) : Boolean; overload;
		function GetParamStr (iIndex: Integer) : String;
        function GetParamUnknownAction : TParamUnknownAction;
		function GetParamValue (iIndex: Integer) : String;
        function GetSeparator : Char;
		function GetValue (const sParam: String) : String; overload;
		function GetValue (sParam: String;
						   out sValue: String) : Boolean; overload;
		function GetValue (const sParam: String;
						   out iValue: Integer) : Boolean; overload;
        function GetValue (const iFixedPos: Integer;
                           out sValue: String) : Boolean; overload;
        function ParamByName (const sName: String) : String;
		function ParamExists (sParam: String) : Boolean; overload;
		function ParamExists (sParam: String;
							  out iIndex: Integer) : Boolean; overload;
		procedure ParseParams;
        procedure SetParamUnknownAction (Action: TParamUnknownAction);

		property ParamStr [iIndex: Integer] : String read GetParamStr;
		property ParamUnknownAction : TParamUnknownAction
									  read GetParamUnknownAction
									  write SetParamUnknownAction;
		property ParamValue [iIndex: Integer] : String read GetParamValue;
        property Seperator : Char read GetSeparator;
    end; { ICmdLineParams }

	TCmdLineParams = class (TInterfacedObject, ICmdLineParams)
	  private
		FParamUnknownAction : TParamUnknownAction;
    	FSeparator : Char;

		aCmdLineParams : array of TCmdLineParam;
        bEmptyValueExists : Boolean;
        sNativeCmdLine : String;

        function FindChildParam (const sParentName: String;
                                 out ChildParam: TCmdLineParam) : Boolean;
		function GetCmdLineParam (sParam: String) : TCmdLineParam;
		function GetCmdLineParamAtPos (const iPos: Integer) : TCmdLineParam;
        function IsFixedParam (iParam: Integer;
                               out CmdLineParam: TCmdLineParam) : Boolean;
		function OnlyOptionalParams : Boolean;
        function RemoveEscapeChars (const sValue: String) : String; overload;
        function ValueContainsEscapeChars (const sValue: String;
                                           const iStartPos: Integer;
                                           out iPos: Integer) : Boolean;

	  public
      	constructor Create (const PUA: TParamUnknownAction;
                            const chSeperator: Char = ':');
		destructor Destroy; override;

		procedure AddParam (const Param: TCmdLineParam); overload;
        function AddParam (const sParamName: String;
        				   const bOptionalParam: Boolean;
                           const ValueKind: TValueKind = vkNoValue;
                           const ValueType: TValueType = vtNone) : TCmdLineParam;
                           											   overload;
        function AddParam (const sParamName: String;
                           const ValueKind: TValueKind = vkNoValue;
                           const ValueType: TValueType = vtNone) : TCmdLineParam;
                           											   overload;
        function AddParam (const sParamName: String;
        				   const iFixedPos: Integer;
                           const bPrefixRequired: Boolean = false;
                           const sParentParam: String = '') : TCmdLineParam;
                                                                       overload;
        function AddParam (const iFixedPos: Integer;
                           const VT: TValueType = vtString;
                           const sParentParam: String = '') : TCmdLineParam;
                                                                       overload;
        function AddParamValue (const sParentParam: String;
                                const VT: TValueType = vtString) : TCmdLineParam;
		function Count : Integer;
        function GetEnumerator : TCmdLineParamsEnumerator;
        function GetParamIndex (const sParam: String) : Integer; overload;
        function GetParamIndex (const sParam: String;
                                out iIndex: Integer) : Boolean; overload;
		function GetParamStr (iIndex: Integer) : String;
        function GetParamUnknownAction : TParamUnknownAction;
		function GetParamValue (iIndex: Integer) : String;
        function GetSeparator : Char;
		function GetValue (const sParam: String) : String; overload;
		function GetValue (sParam: String;
						   out sValue: String) : Boolean; overload;
		function GetValue (const sParam: String;
						   out iValue: Integer) : Boolean; overload;
        function GetValue (const iFixedPos: Integer;
                           out sValue: String) : Boolean; overload;
        function ParamByName (const sName: String) : String;
		function ParamExists (sParam: String) : Boolean; overload;
		function ParamExists (sParam: String;
							  out iIndex: Integer) : Boolean; overload;
		procedure ParseParams;
        procedure SetParamUnknownAction (Action: TParamUnknownAction);

		property ParamStr [iIndex: Integer] : String read GetParamStr;
		property ParamValue [iIndex: Integer] : String read GetParamValue;
		property ParamUnknownAction : TParamUnknownAction
									  read GetParamUnknownAction
									  write SetParamUnknownAction;
        property Seperator : Char read GetSeparator;
    end; { TCmdLineParams }

implementation

uses Win32ToolsU, PasTools;

ResourceString
    cUnknownParamMsg = 'Unknown parameter "%s"';
	cAlreadyExistsErrMsg = 'A parameter "%s" has already been defined';
//	cFixedPosErr = 'Parameter "%s": a fixed position can only be assigned to unnamed parameters';
    cFixedPosAlreadyTakenMsg = 'There is already a fixed parameter at position %d';
    cNoFixedParamAtPrevPosMsg = 'There is no fixed parameter at position %d';
    cNoFixedPosErrMsg = 'An empty parameter must have a fixed position assigned';
    cInvalidParamMsg = '"%s" is not a valid parameter';
    cValueExpectedMsg = 'Invalid parameter "%s"'#13#10 +
        			    '":" must be followed by a value.';
    cNextValueMissingMsg = 'Parameter "%s": associated value missing';
    cNotFoundMsg = 'Required parameter %s not found';
    cNotFoundCommentMsg = 'Required parameter %s ("%s") not found';
    cPositionMsg = 'at position %d';
    cMustBeUsedWithMsg = 'The parameter "%s" can only be used in ' +
        			     'conjunction with the parameter "%s"';
    cCannotBeUsedWithMsg = 'The parameter "%s" can not be used together ' +
        				   'with the parameter "%s"';
    cExpectedMsg = 'The value "%s" of the parameter "%s" does not have the ' +
        		   'expected type';
    cNoValueMsg = 'The parameter "%s" must not have an associated value';
    cValueRequiredMsg = 'The parameter "%s" must have an associated value';
    cFixedParamPosErrMsg =
        'The fixed parameter "%s" must be at position %d instead of %d';
    cUnnamedParamMsg = 'Unnamed parameter at position %d';
    cParentParamNameMissingMsg = 'Parent parameter name missing';
//    cInvalidValueTypeMsg = 'The value type "vtNone" is invalid for this parameter';
    cParentExistsMsg = 'The parameter "%s" already has an assigned parent "%s"';
    cParentMissingMsg = 'The parameter "%s" must have an associated parent';
    cUnknownParamWithParentMsg = '"%s": value missing';
    cRangeErrMsg = '%d is not a valid index';
    cTrue = 'true';
    cFalse = 'false';

const
    cEmptyParam = '""';
    cValueOnly = '$Fixed_Param_';

(* ---- *)

function ParamHasPrefix (const sParam: String;
                         out chPrefix: Char): Boolean; overload;
begin
	if (sParam <> '') and (AnsiChar (sParam [1]) in ['/', '-']) then
    begin
        Result := true;
        chPrefix := sParam [1];
    end { if }
    else Result := false;
end; { ParamHasPrefix }

(* ---- *)

function ParamHasPrefix (const sParam: String): Boolean; overload;

var
    chPrefix : Char;

begin
	Result := ParamHasPrefix (sParam, chPrefix);
end; { ParamHasPrefix }

(* ---- *)

function TCmdLineParam.GetName : String;
begin
    if (Pos (cValueOnly, FName) = 1) then
        Result := Format (cUnnamedParamMsg, [FParamIndex])
    else if (FPrefixRequired) then
        Result := FPrefix + FName
    else Result := FName;
end; { TCmdLineParam.GetName }

(* ---- *)

constructor TCmdLineParam.Create (const sParamName: String;
								  const bOptionalParam: Boolean = false;
								  const bPrefixReq: Boolean = true;
                                  const iFixedPos: Integer = 0;
                                  const sParentParam: String = '');
begin
	Assert (iFixedPos >= (-1));

	Inherited Create;

	FName := sParamName;
	FOptional := bOptionalParam;
	FPrefixRequired := bPrefixReq;
    FFixedPos := iFixedPos;
    FHasFixedPos := iFixedPos <> 0;

    if (sParentParam <> '') then
        FParentParamName := LowerCase (sParentParam);
end; { TCmdLineParam.Create }

(* ---- *)

destructor TCmdLineParam.Destroy;
begin
    inherited;
end; { TCmdLineParam.Destroy }

(* ---- *)

procedure TCmdLineParam.CannotBeUsedWith (const List: array of String);

var
	iIndex : Integer;

begin
	Assert (Length (asCannotBeUsedWith) = 0);

	SetLength (asCannotBeUsedWith, Length (List));

	for iIndex := 0 to High (List) do
		asCannotBeUsedWith [iIndex] := LowerCase (List [iIndex]);
end; { TCmdLineParam.CannotBeUsedWith }

(* ---- *)

procedure TCmdLineParam.MustBeUsedWith (const List: array of String);

var
	iIndex : Integer;
    sStr : String;

begin
	Assert (Length (asMustBeUsedWith) = 0);

	SetLength (asMustBeUsedWith, Length (List));

	for iIndex := 0 to High (List) do
    begin
{$IFNDEF CLR}
//    	sStr := List [iIndex].VPChar;
    	sStr := List [iIndex];
{$ELSE}
    	sStr := List [iIndex] As String;
{$ENDIF}

		asMustBeUsedWith [iIndex] := LowerCase (sStr);
    end; { for }
end; { TCmdLineParam.MustBeUsedWith }

(* ---- *)

procedure TCmdLineParam.SetFixedPos (const iFixedPos: Integer);
begin
    Assert (iFixedPos > 0);

    FFixedPos := iFixedPos;
end; { TCmdLineParam.SetFixedPos }

(* ---- *)

constructor TCmdLineParamsEnumerator.Create (const CmdLineParams: TCmdLineParams);
begin
	inherited Create;

    FCmdLineParams := CmdLineParams;
    FIndex := (-1);
end; { TCmdLineParamsEnumerator.Create }

(* ---- *)

function TCmdLineParamsEnumerator.GetCurrent : TCmdLineParam;
begin
	Result := FCmdLineParams.aCmdLineParams [FIndex];
end; { TCmdLineParamsEnumerator.GetCurrent }

(* ---- *)

function TCmdLineParamsEnumerator.MoveNext : Boolean;
begin
	if (FIndex < High (FCmdLineParams.aCmdLineParams)) then
    begin
		Result := true;
        Inc (FIndex);
    end { if }
    else Result := false;
end; { TCmdLineParamsEnumerator.MoveNext }

(* ---- *)

function TCmdLineParams.FindChildParam (const sParentName: String;
                                        out ChildParam: TCmdLineParam) : Boolean;

var
    iIndex : Integer;
	CmdLineParam : TCmdLineParam;

begin
    Result := false;

	for iIndex := 0 to High (aCmdLineParams) do
	begin
		CmdLineParam := aCmdLineParams [iIndex];

		if (CmdLineParam.FParentParamName = sParentName) then
        begin
            Result := true;
            ChildParam := CmdLineParam;
            Break;
        end; { if }
    end; { for }
end; { TCmdLineParams.FindChildParam }

(* ---- *)

function TCmdLineParams.GetCmdLineParam (sParam: String) : TCmdLineParam;

var
	iIndex : Integer;
	bPrefixExists : Boolean;
	CmdLineParam : TCmdLineParam;
    chPrefix : Char;

begin
	Result := NIL;

	if (ParamHasPrefix (sParam, chPrefix)) then
	begin
		bPrefixExists := true;
		Delete (sParam, 1, 1);
	end { if }
	else bPrefixExists := false;

    sParam := LowerCase (sParam);

	for iIndex := 0 to High (aCmdLineParams) do
	begin
		CmdLineParam := aCmdLineParams [iIndex];

		if (LowerCase (CmdLineParam.FName) = sParam) then
			if (bPrefixExists = CmdLineParam.FPrefixRequired) then
			begin
                if (bPrefixExists) then
                    CmdLineParam.FPrefix := chPrefix;

				Result := CmdLineParam;
				Break;
			end; { if }
	end; { for }
end; { TCmdLineParams.GetCmdLineParam }

(* ---- *)

function TCmdLineParams.GetCmdLineParamAtPos (
                                           const iPos: Integer) : TCmdLineParam;

var
    iIndex : Integer;

begin
	Assert (iPos >= 0);

    Result := NIL;

	if (Count > 0) then
    	for iIndex := 0 to High (aCmdLineParams) do
            if (aCmdLineParams [iIndex].FFixedPos = iPos) then
            begin
                Result := aCmdLineParams [iIndex];
                Break;
            end; { if }
end; { TCmdLineParams.GetCmdLineParamAtPos }

(* ---- *)

function TCmdLineParams.IsFixedParam (iParam: Integer;
                                      out CmdLineParam: TCmdLineParam): Boolean;

var
    iIndex : Integer;

begin
    Assert (iParam >= 0);

    Result := false;

    Inc (iParam);

    for iIndex := 0 to High (aCmdLineParams) do
        if (aCmdLineParams [iIndex].FFixedPos = iParam) then
        begin
            Result := true;
            CmdLineParam := aCmdLineParams [iIndex];
            Break;
        end; { if }
end; { TCmdLineParams.IsFixedParam }

(* ---- *)

function TCmdLineParams.OnlyOptionalParams : Boolean;

var
	iIndex : Integer;

begin
	Result := true;

    if (Count > 0) then
        for iIndex := 0 to High (aCmdLineParams) do
            if (aCmdLineParams [iIndex].FOptional = false) then
            begin
                Result := false;
                Break;
            end; { if }
end; { TCmdLineParams.OnlyOptionalParams }

(* ---- *)

function TCmdLineParams.RemoveEscapeChars (const sValue: String) : String;

var
    iStartPos, iPos : Integer;

begin
    Result := sValue;

    iStartPos := 1;

    while (ValueContainsEscapeChars (Result, iStartPos, iPos)) do
    begin
        Delete (Result, iPos, 1);
        iStartPos := iPos + 1;
    end; { if }
end; { TCmdLineParams.RemoveEscapeChars }

(* ---- *)

function TCmdLineParams.ValueContainsEscapeChars (const sValue: String;
                                                  const iStartPos: Integer;
                                                  out iPos: Integer) : Boolean;
begin
    iPos := NextPos ('^', sValue, iStartPos);

    Result := iPos > 0;
end; { TCmdLineParams.ValueContainsEscapeChars }

(* ---- *)

procedure TCmdLineParams.ParseParams;

    (* ---- *)

    procedure CheckFixedParamsOrder;

        (* ---- *)

        procedure CheckIfPrevParamIsFixed (const iThisParam: Integer);

        var
            iIndex, iPrev : Integer;
            bPrev : Boolean;

        begin
            bPrev := false;
            iPrev := aCmdLineParams [iThisParam].FFixedPos - 1;

            for iIndex := 0 to High (aCmdLineParams) do
                if (aCmdLineParams [iIndex].FFixedPos = iPrev) then
                begin
                    bPrev := true;
                    Break;
                end; { if }

            if not (bPrev) then
                raise EParamCheck.CreateFmt (cNoFixedParamAtPrevPosMsg, [iPrev])
        end; { CheckForPrevFixedParam }

        (* ---- *)

    var
        iIndex : Integer;

    begin { CheckFixedParamsOrder }
        if (Length (aCmdLineParams) > 1) then
            for iIndex := 0 to High (aCmdLineParams) do
                if (aCmdLineParams [iIndex].FFixedPos > 1) and
                   (aCmdLineParams [iIndex].FParentParamName = '') then
                    CheckIfPrevParamIsFixed (iIndex)
                else if (aCmdLineParams [iIndex].FFixedPos = (-1)) then
                    if not (aCmdLineParams [iIndex].FOptional) then
                        raise EParamCheck.CreateFmt (cUnknownParamWithParentMsg,
                                     [aCmdLineParams [iIndex].FParentParamName])
    end; { CheckFixedParamsOrder }

    (* ---- *)

    function CheckForEmptyValue (const Parent, Child: TCmdLineParam) : Boolean;

    var
        iParam, iValue : Integer;
        sParam : String;

    begin
        Result := false;

        sParam := LowerCase (Parent.FPrefix + Parent.FName);

        iParam := Pos (sParam, sNativeCmdLine) + Length (sParam);

        iValue := Pos (cEmptyParam, sNativeCmdLine);

        if (iValue > iParam) then
        begin
            sParam := Copy (sNativeCmdLine, iParam, (iValue + 2) - iParam);

            if (TrimLeft (sParam) = cEmptyParam) then
            begin
                Result := true;
                Child.FEmptyValue := true;
            end; { if }
        end; { if }
    end; { CheckForEmptyValue }

    (* ---- *)

	procedure CheckForRequiredParams;

		(* ---- *)

        function CheckForOtherParam (const asList: TasString;
        							 out iFound: Integer) : Boolean;

        var
        	iIndex : Integer;

        begin
            Result := false;

            if (Length (asList) = 0) then
            	exit;

            for iIndex := 0 to High (asList) do
            begin
            	iFound := iIndex;

            	if (ParamExists (asList [iIndex])) then
                begin
                	Result := true;
                    exit;
                end; { if }
            end; { for }
        end; { CheckForOtherParam }

        (* ---- *)

        procedure RemoveParam (const iParam: Integer);

        var
        	iIndex : Integer;

        begin
        	aCmdLineParams [iParam].Free;

            if not (iParam = High (aCmdLineParams)) then
            	for iIndex := iParam to High (aCmdLineParams) - 1 do
                	aCmdLineParams [iIndex] := aCmdLineParams [iIndex + 1];

            SetLength (aCmdLineParams, Length (aCmdLineParams) - 1);
        end; { RemoveParam }

        (* ---- *)

    var
    	iIndex, iFound : Integer;
        bCheckForMutualExclusive : Boolean;
        Param : TCmdLineParam;

    begin { CheckForRequiredParams }
        for iIndex := High (aCmdLineParams) downto 0 do
        begin
        	Param := aCmdLineParams [iIndex];

            if (Param.FParamExists = false) and (Param.FOptional) then
                Continue;

            bCheckForMutualExclusive :=
                          CheckForOtherParam (Param.asCannotBeUsedWith, iFound);

            if (Param.FParamExists) then
            begin
                if (bCheckForMutualExclusive) then
                    raise EParamCheck.CreateFmt (cCannotBeUsedWithMsg,
                                      [Param.FName,
                                       Param.asCannotBeUsedWith [iFound]]);

                if (Length (Param.asMustBeUsedWith) > 0) then
                    if not (CheckForOtherParam (Param.asMustBeUsedWith,
                                                iFound)) then
                        raise EParamCheck.CreateFmt (cMustBeUsedWithMsg,
                                        	  [Param.FName,
                                        	   Param.asMustBeUsedWith [iFound]]);
            end { if }
            else if (bCheckForMutualExclusive) then
            	RemoveParam (iIndex)
            else if (Param.FParentParam <> NIL) and
            		(CheckForEmptyValue (Param.FParentParam, Param)) then
            begin
            	Param.FParamExists := true;
                Param.FEmptyValue := true;
            end { else if }
            else
            begin
                if (Pos (cValueOnly, Param.FName) = 1) then
                    Param.FName := Format (cPositionMsg, [Param.FFixedPos])
                else Param.FName := '"' + Param.FName + '"';

                if (Param.Comment <> '') then
                    raise EParamCheck.CreateFmt (cNotFoundCommentMsg,
                                                 [Param.FName, Param.Comment])
                else raise EParamCheck.CreateFmt (cNotFoundMsg, [Param.FName]);
            end; { else }
        end; { for }
    end; { CheckForRequiredParams }

    (* ---- *)

    procedure CheckValues;

        (* ---- *)

        procedure CheckValueType (const sName, sValue: String;
                                  const ValueType: TValueType);

        var
            iValue, iCode : Integer;

        begin
            case ValueType of
                vtInteger :
                    begin
                        Val (sValue, iValue, iCode);

                        if (iCode = 0) then
                            exit;

                        iCode := iValue;
                    end; { case vtInteger }

                vtBoolean :
                    if (LowerCase (sValue) = cTrue) or
                       (LowerCase (sValue) = cFalse) then
                        exit;

                else exit; // String
            end; { case }

            raise EParamCheck.CreateFmt (cExpectedMsg, [sValue, sName]);
        end; { CheckValueType }

        (* ---- *)

    var
    	iIndex : Integer;
        ParentParam, ChildParam : TCmdLineParam;

    begin { CheckValues }
        for iIndex := 0 to High (aCmdLineParams) do
        begin
            ParentParam := aCmdLineParams [iIndex];

            if (ParentParam.FParamExists = false) and
               (ParentParam.FOptional) then
                Continue;

            case ParentParam.FValueKind of
                vkNoValue :
                    if (ParentParam.FValue <> '') then
                        raise EParamCheck.CreateFmt (cNoValueMsg,
                                                     [ParentParam.FName]);

                vkOptionalValue :
                    if (ParentParam.FValue <> '') then
                        CheckValueType (ParentParam.FName, ParentParam.FValue,
                                        ParentParam.FValueType);

                vkValueRequired :
                    if (ParentParam.FValue = '') then
                    begin
                    	if not (ParentParam.FEmptyValue) then
	                        raise EParamCheck.CreateFmt (cValueRequiredMsg,
    	                                                 [ParentParam.FName])
                    end { if }
                    else CheckValueType (ParentParam.FName, ParentParam.FValue,
                                         ParentParam.FValueType);

                vkNextParam :
                    begin
                        if not (FindChildParam (LowerCase (ParentParam.FName),
                                                ChildParam)) then
                            ChildParam := NIL
                        else if (ChildParam.FValue = '') then
                            if (bEmptyValueExists) then
                            begin
                                if not (CheckForEmptyValue (ParentParam,
                                                            ChildParam)) then
                                    ChildParam := NIL
                            end { if }
                            else ChildParam := NIL;

                        if (ChildParam = NIL) then
                            raise EParamCheck.CreateFmt (cNextValueMissingMsg,
                                                         [ParentParam.FName]);
                    end; { case vkNextParam }
            end; { case }

            if (Assigned (ParentParam.OnCheckParam)) then
                ParentParam.OnCheckParam (ParentParam.FName, ParentParam.FValue)
        end; { for }
    end; { CheckValues }

	(* ---- *)

    procedure FillCmdArray (out asActualParams, asParamValues: TasString);

    var
    	sParam : String;
        iIndex, iPos : Integer;

    begin
        SetLength (asActualParams{%H-}, ParamCount);
        SetLength (asParamValues{%H-}, ParamCount);

    	if (ParamCount = 0) then
        	exit;

        for iIndex := 1 to ParamCount do
        begin
			sParam := System.ParamStr (iIndex);

			if (ParamHasPrefix (sParam)) then
			begin
				iPos := Pos (FSeparator, sParam);

				if (iPos > 1) then
				begin
					if (iPos = Length (sParam)) then
						raise EParamCheck.CreateFmt (cValueExpectedMsg,
                                                     [System.ParamStr (iIndex)]);

					asParamValues [iIndex - 1] := Copy (sParam, iPos + 1,
														Length (sParam) - iPos);

					SetLength (sParam, iPos - 1);
				end { if }
				else if (sParam = '') or (iPos = 1) then
					raise EParamCheck.CreateFmt (cInvalidParamMsg,
                                                 [System.ParamStr (iIndex)])
				else asParamValues [iIndex - 1] := '';
			end { if }
            else if (GetCmdLineParamAtPos (iIndex) <> NIL) then
                asParamValues [iIndex - 1] := System.ParamStr (iIndex)
			else asParamValues [iIndex - 1] := '';

			asActualParams [iIndex - 1] := sParam;
		end; { for }
	end; { FillCmdArray }

	(* ---- *)

	procedure SetCmdLineParams (const asActualParams, asParamValues: TasString);

    var
    	iIndex : Integer;
        CmdLineParam : TCmdLineParam;
        bFixedParam : Boolean;

    begin
    	for iIndex := 0 to High (asActualParams) do
        begin
            bFixedParam := false;

        	CmdLineParam := GetCmdLineParam (asActualParams [iIndex]);

            if (Assigned (CmdLineParam)) then
            begin
                if (CmdLineParam.FHasFixedPos) then
                begin
                    if (CmdLineParam.FFixedPos <> Succ (iIndex)) then
                        raise EParamCheck.CreateFmt (cFixedParamPosErrMsg,
                                                     [asActualParams [iIndex],
                                                      CmdLineParam.FFixedPos,
                                                      Succ (iIndex)]);
                end { if }
                else
                    if (CmdLineParam.FChildParam <> NIL) then
                        CmdLineParam.FChildParam.FFixedPos := iIndex + 2;
            end { if }
            else
                if (IsFixedParam (iIndex, CmdLineParam)) then
                    bFixedParam := true
                else CmdLineParam := NIL;

            if (CmdLineParam = NIL) then
            	if (ParamUnknownAction = puaError) then
					raise EParamCheck.CreateFmt (cUnknownParamMsg,
                                                 [asActualParams [iIndex]])
                else Continue;

            with CmdLineParam do
			begin
				FParamExists := true;
				FParamIndex := iIndex + 1;

                if (bFixedParam) then
                begin
                    FValue := asActualParams [iIndex];

                    if (ParentParam <> NIL) then
                        ParentParam.FValue := FValue;
                end { if }
				else FValue := asParamValues [iIndex];

                if (FValue <> '') then
                    if (RemoveEscapeChars (FValue) = '') then
                        raise EParamCheck.CreateFmt (cInvalidParamMsg,
                                                 [System.ParamStr (iIndex + 1)])
			end; { with }
		end; { for }
	end; { SetCmdLineParams }

    (* ---- *)

    procedure MatchChildrenAndParents;

    var
        ChildParam : TCmdLineParam;
        iIndex : Integer;
        sName : String;

    begin
        for iIndex := 0 to High (aCmdLineParams) do
            if (aCmdLineParams [iIndex].FValueKind = vkNextParam) then
            begin
                sName := aCmdLineParams [iIndex].FName;

                if (FindChildParam (LowerCase (sName), ChildParam)) then
                begin
                    if (ChildParam.FParentParam <> NIL) then
                        raise EParamCheck.CreateFmt (cParentExistsMsg,
                                                     [ChildParam.FValue, sName]);

                    ChildParam.FParentParam := aCmdLineParams [iIndex];
                    ChildParam.FOptional := ChildParam.FParentParam.FOptional;
                    aCmdLineParams [iIndex].FChildParam := ChildParam;
                end { if }
                else raise EParamCheck.CreateFmt (cNextValueMissingMsg,
                                                  [aCmdLineParams [iIndex].FName]);
            end; { if }

        for iIndex := 0 to High (aCmdLineParams) do
            if (aCmdLineParams [iIndex].FParentParamName <> '') and
               (aCmdLineParams [iIndex].FParentParam = NIL) then
                raise EParamCheck.CreateFmt (cParentMissingMsg,
                                     [aCmdLineParams [iIndex].FParentParamName])
    end; { MatchChildrenAndParents }

    (* ---- *)

    procedure RemoveEscapeCharacters (const asParamValues: TasString);

    var
        iIndex, iPos, iStartPos : Integer;

    begin
        for iIndex := 0 to High (asParamValues) do
        begin
            if (asParamValues [iIndex] = '') then
                Continue;

            iStartPos := 1;

            while (ValueContainsEscapeChars (asParamValues [iIndex], iStartPos,
                                             iPos)) do
            begin
                Delete (asParamValues [iIndex], iPos, 1);
                iStartPos := iPos;
            end; { if }

            if (asParamValues [iIndex] = '') then
                raise EParamCheck.CreateFmt (cInvalidParamMsg,
                                             [System.ParamStr (iIndex + 1)])
        end; { for }
    end; { RemoveEscapeCharacters }

    (* ---- *)

var
	asActualParams, asParamValues : TasString;

begin { TCmdLineParams.ParseParams }
	FillCmdArray (asActualParams, asParamValues);

    if (ParamCount = 0) then
    	if (Count = 0) or (OnlyOptionalParams) then
        	exit;

    RemoveEscapeCharacters (asParamValues);

    MatchChildrenAndParents;
    SetCmdLineParams (asActualParams, asParamValues);
    CheckFixedParamsOrder;
    CheckForRequiredParams;
    CheckValues;
end; { TCmdLineParams.ParseParams }

(* ---- *)

procedure TCmdLineParams.SetParamUnknownAction (Action: TParamUnknownAction);
begin
    FParamUnknownAction := Action;
end; { TCmdLineParams.SetParamUnknownAction }

(* ---- *)

constructor TCmdLineParams.Create (const PUA: TParamUnknownAction;
                                   const chSeperator: Char = ':');
begin
	inherited Create;

    FParamUnknownAction := PUA;
    FSeparator := chSeperator;

    sNativeCmdLine := LowerCase (GetParams);

    if (Pos (cEmptyParam, sNativeCmdLine) > 0) then
        bEmptyValueExists := true;
end; { TCmdLineParams.Create }

(* ---- *)

destructor TCmdLineParams.Destroy;

var
	iIndex : Integer;

begin
	if (Count > 0) then
    	for iIndex := 0 to High (aCmdLineParams) do
        	aCmdLineParams [iIndex].Free;

	inherited;
end; { TCmdLineParams.Destroy }

(* ---- *)

procedure TCmdLineParams.AddParam (const Param: TCmdLineParam);

var
    iIndex : Integer;

begin
    if (Param.FName = '') then
    begin
        if not (Param.FHasFixedPos) then
            raise EParamCheck.Create (cNoFixedPosErrMsg);

        if (Param.FFixedPos > 0) then
            Param.FName := cValueOnly + IntToStr (Param.FFixedPos)
        else Param.FName := cValueOnly + Param.FParentParamName;

        Param.FValueKind := vkValueRequired;
        Param.FValueType := vtString;
    end { if }
	else
        if (ParamExists (Param.FName)) then
            raise EParamCheck.CreateFmt (cAlreadyExistsErrMsg, [Param.FName]);

	iIndex := Length (aCmdLineParams);
	SetLength (aCmdLineParams, iIndex + 1);
    aCmdLineParams [iIndex] := Param;
end; { TCmdLineParams.AddParam }

(* ---- *)

function TCmdLineParams.AddParam (const sParamName: String;
								  const bOptionalParam: Boolean;
                                  const ValueKind: TValueKind = vkNoValue;
                          const ValueType: TValueType = vtNone) : TCmdLineParam;
begin
	Result := TCmdLineParam.Create (sParamName);
    Result.FOptional := bOptionalParam;
    Result.ValueKind := ValueKind;
    Result.ValueType := ValueType;

    AddParam (Result);
end; { TCmdLineParams.AddParam }

(* ---- *)

function TCmdLineParams.AddParam (const sParamName: String;
								  const ValueKind: TValueKind = vkNoValue;
                          const ValueType: TValueType = vtNone) : TCmdLineParam;
begin
	Result := AddParam (sParamName, false, ValueKind, ValueType);
end; { TCmdLineParams.AddParam }

(* ---- *)

function TCmdLineParams.AddParam (const sParamName: String;
								  const iFixedPos: Integer;
                                  const bPrefixRequired: Boolean = false;
                               const sParentParam: String = '') : TCmdLineParam;

    (* ---- *)

    function IsFixedPosAlreadyTaken : Boolean;

    var
        iIndex : Integer;

    begin
        for iIndex := 0 to High (aCmdLineParams) do
            if (aCmdLineParams [iIndex].FFixedPos > 0) then
                if (aCmdLineParams [iIndex].FFixedPos = iFixedPos) then
                begin
                    Result := true;
                    exit;
                end; { if }

        Result := false
    end; { IsFixedPosAlreadyTaken }

    (* ---- *)

begin { TCmdLineParams.AddParam }
    Assert (iFixedPos >= (-1));

    if (IsFixedPosAlreadyTaken) then
        raise EParamCheck.CreateFmt (cFixedPosAlreadyTakenMsg, [iFixedPos]);

    Result := TCmdLineParam.Create (sParamName, false, bPrefixRequired,
                                    iFixedPos, sParentParam);
    AddParam (Result);
end; { TCmdLineParams.AddParam }

(* ---- *)

function TCmdLineParams.AddParam (const iFixedPos: Integer;
                                  const VT: TValueType = vtString;
                               const sParentParam: String = '') : TCmdLineParam;
begin
    Assert ((iFixedPos > 0) or (iFixedPos = (-1)));

    Result := AddParam ('', iFixedPos, false, sParentParam);
    Result.FValueType := VT;
    Result.FValueKind := vkValueRequired;
end; { TCmdLineParams.AddParam }

(* ---- *)

function TCmdLineParams.AddParamValue (const sParentParam: String;
                               const VT: TValueType = vtString) : TCmdLineParam;
begin
    if (sParentParam = '') then
        raise EParamCheck.Create (cParentParamNameMissingMsg);

    Result := AddParam ((-1), VT, sParentParam);
    Result.FValueType := VT;
    Result.FValueKind := vkValueRequired;
end; { TCmdLineParams.AddParamValue }

(* ---- *)

function TCmdLineParams.Count : Integer;
begin
	Result := Length (aCmdLineParams);
end; { TCmdLineParams.Count }

(* ---- *)

function TCmdLineParams.GetEnumerator : TCmdLineParamsEnumerator;
begin
	Result := TCmdLineParamsEnumerator.Create (Self);
end; { TCmdLineParams.GetEnumerator }

(* ---- *)

function TCmdLineParams.GetParamIndex (const sParam: String) : Integer;
begin
    Assert (sParam <> '');

    if not (ParamExists (sParam, Result)) then
        if (FParamUnknownAction = puaError) then
		    raise EParamCheck.CreateFmt (cUnknownParamMsg, [sParam])
end; { TCmdLineParams.GetParamIndex }

(* ---- *)

function TCmdLineParams.GetParamIndex (const sParam: String;
                                       out iIndex: Integer) : Boolean;
begin
    Assert (sParam <> '');

    Result := ParamExists (sParam, iIndex);
end; { TCmdLineParams.GetParamIndex }

(* ---- *)

function TCmdLineParams.GetParamStr (iIndex: Integer) : String;
begin
	Assert (iIndex >= 0);

	if (iIndex >= Count) then
	begin
		Result := '';
		exit;
	end; { if }

	Result := aCmdLineParams [iIndex].FName;
end; { TCmdLineParams.GetParamStr }

(* ---- *)

function TCmdLineParams.GetParamUnknownAction : TParamUnknownAction;
begin
    Result := FParamUnknownAction;
end; { TCmdLineParams.GetParamUnknownAction }

(* ---- *)

function TCmdLineParams.GetParamValue (iIndex: Integer) : String;
begin
	Assert (iIndex >= 0);

	if (iIndex < 0) or (iIndex >= Count) then
        raise EParamCheck.CreateFmt (cRangeErrMsg, [iIndex]);

    Result := aCmdLineParams [iIndex].FValue;
end; { TCmdLineParams.GetParamValue }

(* ---- *)

function TCmdLineParams.GetSeparator : Char;
begin
    Result := FSeparator;
end; { TCmdLineParams.GetSeparator }

(* ---- *)

function TCmdLineParams.GetValue (const sParam: String) : String;
begin
    GetValue (sParam, Result);
end; { TCmdLineParams.GetValue }

(* ---- *)

function TCmdLineParams.GetValue (sParam: String;
								  out sValue: String) : Boolean;

var
    iIndex : Integer;

begin
	Assert (sParam <> '');

    sValue := '';
    sParam := LowerCase (sParam);

    for iIndex := 0 to High (aCmdLineParams) do
    	if (sParam = LowerCase (aCmdLineParams [iIndex].FName)) then
        begin
            sValue := RemoveEscapeChars (aCmdLineParams [iIndex].FValue);
            Result := sValue <> '';
            exit;
        end; { if }

    Result := false;
end; { TCmdLineParams.GetValue }

(* ---- *)

function TCmdLineParams.GetValue (const sParam: String;
								  out iValue: Integer) : Boolean;

var
	sValue : String;
    iCode : Integer;

begin
	Assert (sParam <> '');

	Result := GetValue (sParam, sValue);

    if (Result) then
    begin
    	Val (sValue, iValue, iCode);
        Result := iCode = 0;
    end; { if }
end; { TCmdLineParams.GetValue }

(* ---- *)

function TCmdLineParams.GetValue (const iFixedPos: Integer;
                                  out sValue: String) : Boolean;

var
    CmdLineParam : TCmdLineParam;

begin
	Assert (iFixedPos > 0);

    Result := false;
    sValue := '';

	if (iFixedPos > Count) then
    	exit;

    if (aCmdLineParams [iFixedPos - 1].FFixedPos = 0) then
        exit;

    CmdLineParam := GetCmdLineParamAtPos (iFixedPos);

    if (CmdLineParam <> NIL) then
    begin
        Result := true;
        sValue := CmdLineParam.FValue;
    end; { if }
end; { TCmdLineParams.GetValue }

(* ---- *)

function TCmdLineParams.ParamByName (const sName: String) : String;
begin
	Assert (sName <> '');

	if not (GetValue (sName, Result)) then
		raise EParamCheck.CreateFmt (cUnknownParamMsg, [sName])
end; { TCmdLineParams.ParamByName }

(* ---- *)

function TCmdLineParams.ParamExists (sParam: String) : Boolean;

var
	iIndex : Integer;

begin
	Assert (sParam <> '');

	Result := ParamExists (sParam, iIndex);
end; { TCmdLineParams.ParamExists }

(* ---- *)

function TCmdLineParams.ParamExists (sParam: String;
									 out iIndex: Integer) : Boolean;

var
    iParam : Integer;

begin
	Assert (sParam <> '');

    sParam := LowerCase (sParam);

    for iParam := 0 to High (aCmdLineParams) do
    	with aCmdLineParams [iParam] do
            if (sParam = LowerCase (FName)) and (FParamExists) then
            begin
                Result := true;
            	iIndex := iParam;
                exit;
            end; { if }

    Result := false;
end; { TCmdLineParams.ParamExists }

(* ---- *)

end.
