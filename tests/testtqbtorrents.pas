unit TestTqBTorrents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  qBTorrents;

type

  TTestTqBTorrents = class(TTestCase)
  private
    FqBTorrents: TqBTorrents;
  published
    procedure TestTorrentsCreate;
  end;

implementation

procedure TTestTqBTorrents.TestTorrentsCreate;
begin
  FqBTorrents := TqBTorrents.Create;
  try
    AssertEquals('Torrents Count 0', 0, FqBTorrents.Count);
  finally
    FqBTorrents.Free;
  end;
end;



initialization

  RegisterTest(TTestTqBTorrents);
end.

