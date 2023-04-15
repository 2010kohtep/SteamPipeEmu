unit Xander.Helpers;

interface

uses
  System.SysUtils, System.JSON;

type
  TPointerHelper = record helper for Pointer
  public
    function ToInteger: Integer;
    function ToString: string;
  end;

type
  TJSONObjectHelper = class helper for TJSONObject
  private
    function GetThis: TJSONObject; inline;
  public
    property This: TJSONObject read GetThis;
  end;

type
  TArchitectureHelper = record helper for TOSVersion.TArchitecture
  public
    function ToString: string;
  end;

implementation

{ TPointerHelper }

function TPointerHelper.ToInteger: Integer;
begin
  Result := Integer(Self);
end;

function TPointerHelper.ToString: string;
begin
  Result := Self.ToInteger.ToString;
end;

{ TArchitectureHelper }

function TArchitectureHelper.ToString: string;
begin
  case Self of
    arIntelX86: Exit('Intel x86');
    arIntelX64: Exit('Intel x64');
    arARM32: Exit('ARM x32');
    arARM64: Exit('ARM x64');
  else
    Result := 'Unknown';
  end;
end;

{ TJSONObjectHelper }

function TJSONObjectHelper.GetThis: TJSONObject;
begin
  Result := Self;
end;

end.
