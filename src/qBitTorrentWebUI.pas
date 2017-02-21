{
  qBitTorrentWebUI component.

  Copyright (c) 2017 Gustavo Carreno <guscarreno@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit qBitTorrentWebUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPSend, qBTorrents;

type
{ TqBitTorrentWebUI }
  TqBitTorrentWebUI = class(TComponent)
  private
    FActive: Boolean;
    FUserName: String;
    FPassword: String;
    FHost: String;
    FPort: Integer;
    FHttp: THTTPSend;
    FIsLogged: Boolean;
    FLoginCookie: String;
    FAPIVersion: Integer;
    FMinAPIVersion: Integer;
    FqBitTorrentVersion: String;
    FTorrents: TqBTorrents;

    procedure SetActive(aValue: Boolean);

    // Authentication
    function DoLogin: Boolean;
    function DoLogout: Boolean;

    // Get methods
    function DoGetApiVersion: String;
    function DoGetMinApiVersion: String;
    function DoGetqBitTorrentVersion: String;

    // Commands
    function DoExecShutdown: Boolean;

    // Queries
    function DoGetTorrents: Boolean;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;

    // Get methods
    function ExecShutdown: Boolean;
    function GetTorrents: Boolean;

    property IsLogged: Boolean
      read FIsLogged;
    property LoginCookie: String
      read FLoginCookie;
    property APIVersion: Integer
      read FAPIVersion;
    property MinAPIVersion: Integer
      read FMinAPIVersion;
    property qBitTorrentVersion: String
      read FqBitTorrentVersion;
    property Torrents: TqBTorrents
      read FTorrents;
  published
    property Active: Boolean
      read FActive
      write SetActive
      default False;
    property UserName: String
      read FUserName
      write FUserName;
    property Password: String
      read FPassword
      write FPassword;
    property Host: String
      read FHost
      write FHost;
    property Port: Integer
      read FPort
      write FPort
      default 8080;
    // TODO: Add some Notifications for HTTP access:
    // Like Login and such
  end;

implementation

uses
  fpjson, jsonparser, jsonscanner;

const
  sUserAgent = 'lazqBitTorrentWebUI/0.1.0.6';

{ TqBitTorrentWebUI }

constructor TqBitTorrentWebUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FUserName := 'admin';
  FPassword := 'admin';
  FHost := '127.0.0.1';
  FPort := 8080;
  FHttp := THTTPSend.Create;
  FIsLogged := False;
  FLoginCookie := '';
  FAPIVersion := -1;
  FMinAPIVersion := -1;
  FqBitTorrentVersion := '';
  FTorrents := TqBTorrents.Create(True);
end;

destructor TqBitTorrentWebUI.Destroy;
begin
  FTorrents.Free;
  FHttp.Free;
  inherited Destroy;
end;

procedure TqBitTorrentWebUI.SetActive(aValue: Boolean);
begin
  if FActive = aValue then exit;
  case aValue of
    True:begin
      // Add code to Login and get Versions
      DoLogin;
      FAPIVersion := StrToInt(DoGetApiVersion);
      FMinAPIVersion := StrToInt(DoGetMinApiVersion);
      FqBitTorrentVersion := DoGetqBitTorrentVersion;
      FActive := True;
    end;
    False:begin
      // Add code to clear data and Logout
      DoLogout;
      FActive := False;
      FIsLogged := False;
      Clear;
    end;
  end;
end;

procedure TqBitTorrentWebUI.Clear;
begin
  FLoginCookie := '';
  FAPIVersion := -1;
  FMinAPIVersion := -1;
  FqBitTorrentVersion := '';
  FTorrents.Clear;
end;

function TqBitTorrentWebUI.DoLogin: Boolean;
var
  sURL: String;
  sDoc: TStringStream;
  sCookie: String;
  index: Integer;
  iPos: Integer;
const
  sPath = '/login';
begin
  Result := False;
  FHttp.Clear;
  FHttp.UserAgent := sUserAgent;
  FHttp.Headers.Add('Content-Type: application/x-www-form-urlencoded');
  sDoc := TStringStream.Create('username='+FUserName+'&password='+FPassword);
  FHttp.Document.LoadFromStream(sDoc);
  sDoc.Free;
  if FPort = 80 then
  begin
    sURL := 'http://'+FHost+sPath;
  end
  else
  begin
    sURL := 'http://'+FHost+':'+IntToStr(FPort)+sPath;
  end;
  FHttp.HTTPMethod('POST', sURL);
  if FHttp.ResultCode = 200 then
  begin
    Result := True;
    FIsLogged := True;
    sCookie := '';
    FLoginCookie := '';
    for index := 0 to FHttp.Cookies.Count - 1 do
    begin
      sCookie := FHttp.Cookies[index];
      iPos := Pos('SID=', sCookie);
      if iPos > 0 then
      begin
        FLoginCookie := Copy(sCookie, iPos+4, MaxInt);
        Break;
      end;
    end;
  end
  else
  begin
    raise Exception.Create(
      'Login failed: '+IntToStr(FHttp.ResultCode)+' '+FHttp.ResultString
    );
  end;
end;

function TqBitTorrentWebUI.DoLogout: Boolean;
var
  sURL: String;
const
  sPath = '/logout';
begin
  Result := False;
  FHttp.Clear;
  FHttp.UserAgent := sUserAgent;
  FHttp.Cookies.Add('SID='+FLoginCookie);
  if FPort = 80 then
  begin
    sURL := 'http://'+FHost+sPath;
  end
  else
  begin
    sURL := 'http://'+FHost+':'+IntToStr(FPort)+sPath;
  end;
  FHttp.HTTPMethod('POST', sURL);
  if FHttp.ResultCode = 200 then
  begin
    Result := True;
    FIsLogged := False;
  end
  else
  begin
    raise Exception.Create(
      'Logout failed: '+IntToStr(FHttp.ResultCode)+' '+FHttp.ResultString
    );
  end;
end;

function TqBitTorrentWebUI.DoGetApiVersion: String;
var
  sURL: String;
  sVer: TStringStream;
const
  sPath = '/version/api';
begin
  Result := '';
  FHttp.Clear;
  FHttp.UserAgent := sUserAgent;
  FHttp.Cookies.Add('SID='+FLoginCookie+';');
  if FPort = 80 then
  begin
    sURL := 'http://'+FHost+sPath;
  end
  else
  begin
    sURL := 'http://'+FHost+':'+IntToStr(FPort)+sPath;
  end;
  FHttp.HTTPMethod('GET', sURL);
  if FHttp.ResultCode = 200 then
  begin
    sVer := TStringStream.Create;
    FHttp.Document.SaveToStream(sVer);
    Result := sVer.DataString;
    sVer.Free;
  end
  else
  begin
    raise Exception.Create(
      'API Version failed: '+IntToStr(FHttp.ResultCode)+' '+FHttp.ResultString
    );
  end;
end;

function TqBitTorrentWebUI.DoGetMinApiVersion: String;
var
  sURL: String;
  sVer: TStringStream;
const
  sPath = '/version/api_min';
begin
  Result := '';
  FHttp.Clear;
  FHttp.UserAgent := sUserAgent;
  FHttp.Cookies.Add('SID='+FLoginCookie+';');
  if FPort = 80 then
  begin
    sURL := 'http://'+FHost+sPath;
  end
  else
  begin
    sURL := 'http://'+FHost+':'+IntToStr(FPort)+sPath;
  end;
  FHttp.HTTPMethod('GET', sURL);
  if FHttp.ResultCode = 200 then
  begin
    sVer := TStringStream.Create;
    FHttp.Document.SaveToStream(sVer);
    Result := sVer.DataString;
    sVer.Free;
  end
  else
  begin
    raise Exception.Create(
      'Minimum API Version failed: '+IntToStr(FHttp.ResultCode)+' '+FHttp.ResultString
    );
  end;
end;

function TqBitTorrentWebUI.DoGetqBitTorrentVersion: String;
var
  sURL: String;
  sVer: TStringStream;
const
  sPath = '/version/qbittorrent';
begin
  Result := '';
  FHttp.Clear;
  FHttp.UserAgent := sUserAgent;
  FHttp.Cookies.Add('SID='+FLoginCookie+';');
  if FPort = 80 then
  begin
    sURL := 'http://'+FHost+sPath;
  end
  else
  begin
    sURL := 'http://'+FHost+':'+IntToStr(FPort)+sPath;
  end;
  FHttp.HTTPMethod('GET', sURL);
  if FHttp.ResultCode = 200 then
  begin
    sVer := TStringStream.Create;
    FHttp.Document.SaveToStream(sVer);
    Result := sVer.DataString;
    sVer.Free;
  end
  else
  begin
    raise Exception.Create(
      'qBitTorrent Version failed: '+IntToStr(FHttp.ResultCode)+' '+FHttp.ResultString
    );
  end;
end;

function TqBitTorrentWebUI.DoExecShutdown: Boolean;
var
  sURL: String;
const
  sPath = '/command/shutdown';
begin
  Result := False;
  FHttp.Clear;
  FHttp.UserAgent := sUserAgent;
  FHttp.Cookies.Add('SID='+FLoginCookie);
  if FPort = 80 then
  begin
    sURL := 'http://'+FHost+sPath;
  end
  else
  begin
    sURL := 'http://'+FHost+':'+IntToStr(FPort)+sPath;
  end;
  FHttp.HTTPMethod('GET', sURL);
  if FHttp.ResultCode = 200 then
  begin
    Result := True;
  end
  else
  begin
    raise Exception.Create(
      'Shutdown failed: '+IntToStr(FHttp.ResultCode)+' '+FHttp.ResultString
    );
  end;
end;

function TqBitTorrentWebUI.ExecShutdown: Boolean;
begin
  if FActive then
  begin
    Result := DoExecShutdown;
    if Result then
      Active := False;
  end
  else
  begin
    Result := False;
    raise Exception.Create(
      'You need to set Active True first.'
    );
  end;
end;

function TqBitTorrentWebUI.DoGetTorrents: Boolean;
var
  sURL: String;
  jParser: TJSONParser;
  jData: TJSONData;
  jTorrents: TJSONArray;
  jTorrent: TJSONObject;
  index: Integer;
  oTorrent: TqBTorrent;
const
  sPath = '/query/torrents';
begin
  Result := False;
  FTorrents.Clear;
  FHttp.Clear;
  FHttp.UserAgent := sUserAgent;
  FHttp.Cookies.Add('SID='+FLoginCookie);
  if FPort = 80 then
  begin
    sURL := 'http://'+FHost+sPath;
  end
  else
  begin
    sURL := 'http://'+FHost+':'+IntToStr(FPort)+sPath;
  end;
  FHttp.HTTPMethod('GET', sURL);
  if FHttp.ResultCode = 200 then
  begin
    Result := True;
    // Implement getting the shtuff
    try
      jParser := TJSONParser.Create(FHttp.Document, [joUTF8, joIgnoreTrailingComma]);
      jData := jParser.Parse;
    finally
      jParser.Free;
    end;
    if jData.JSONType = jtArray then
    begin
      jTorrents := jData as TJSONArray;
      for index := 0 to jTorrents.Count - 1 do
      begin
        if jTorrents[index].JSONType = jtObject then
        begin
          jTorrent := jTorrents[index] as TJSONObject;
          oTorrent := TqBTorrent.Create;
          oTorrent.Hash := jTorrent.Strings['hash'];
          oTorrent.Name := jTorrent.Strings['name'];
          FTorrents.Add(oTorrent);
        end;
      end;
    end;
  end
  else
  begin
    raise Exception.Create(
      'Getting torrents failed: '+IntToStr(FHttp.ResultCode)+' '+FHttp.ResultString
    );
  end;
end;

function TqBitTorrentWebUI.GetTorrents: Boolean;
begin
  if FActive then
  begin
    Result := DoGetTorrents;
  end
  else
  begin
    Result := False;
    raise Exception.Create(
      'You need to set Active True first.'
    );
  end;
end;

end.
