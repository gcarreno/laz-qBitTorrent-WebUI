unit TestTqBTorrents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  qBTorrents;

type

  { TTestTqBTorrents }

  TTestTqBTorrents = class(TTestCase)
  private
    FqBTorrents: TqBTorrents;
  published
    procedure TestTorrentsCreate;
    procedure TestTorrentsLoadFromJSON;
  end;

implementation

procedure TTestTqBTorrents.TestTorrentsCreate;
begin
  FqBTorrents := TqBTorrents.Create(True);
  try
    AssertEquals('Torrents Count 0', 0, FqBTorrents.Count);
  finally
    FqBTorrents.Free;
  end;
end;

procedure TTestTqBTorrents.TestTorrentsLoadFromJSON;
var
  sFileName: String;
  slJSON: TStringList;
begin
  sFileName := ExtractFileDir(ParamStr(0));
  sFileName := sFileName + '/../tests/data/torrents.json';
  slJSON := TStringList.Create;
  try
    slJSON.LoadFromFile(sFileName);
    FqBTorrents := TqBTorrents.Create(True);
    try
      FqBTorrents.LoadTorrents(slJSON.Text);
      AssertEquals('Loaded torrents 3', 3, FqBTorrents.Count);
    finally
      FqBTorrents.Free;
    end;
  finally
    slJSON.Free;
  end;
end;

initialization
  RegisterTest(TTestTqBTorrents);
end.

