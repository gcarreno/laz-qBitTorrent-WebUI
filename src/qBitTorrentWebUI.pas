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
  Classes, SysUtils, HTTPSend,
  qBTorrents, qBTorrentsFilters{, qBTorrentsProperties}{, qBTorrentsTrackers};

const
  csVersion = '0.16.7.1';

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
    FAPIVersion: String;
    FqBitTorrentVersion: String;
    FRefreshID: Integer;

    FTorrents: TqBTorrents;

    procedure SetActive(aValue: Boolean);
    function GetVersion: String;

    // Authentication
    function DoLogin: Boolean;
    function DoLogout: Boolean;

    // Application
    function DoGetApiVersion: String;
    function DoGetqBitTorrentVersion: String;
    function DoExecShutdown: Boolean;

    // Torrents
    function DoGetTorrents(const aFilter: TqBTorrentsFilter): Boolean;
    function DoGetTorrentProperties(const aHash: String): Boolean;
    function DoGetTorrentTrackers(const aHash: String): Boolean;
    function DoGetTorrentWebSeeds(const aHash: String): Boolean;
    function DoGetTorrentFiles(const aHash: String): Boolean;
    function DoPauseTorrentsAll: Boolean;
    function DoPauseTorrents(const aHashes: TStringList): Boolean;
    function DoResumeTorrentsAll: Boolean;
    function DoResumeTorrents(const aHashes: TStringList): Boolean;


  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;

    // Application
    function ExecShutdown: Boolean;

    // Torrents
    function GetTorrents: Boolean;
    function GetTorrentsFiltered(const aFilter: TqBTorrentsFilter): Boolean;
    function GetTorrentProperties(const aHash: String): Boolean;
    function GetTorrentTrackers(const aHash: String): Boolean;
    function GetTorrentWebSeeds(const aHash: String): Boolean;
    function GetTorrentFiles(const aHash: String): Boolean;
    function PauseTorrentsAll: Boolean;
    function PauseTorrents(const aHashes: TStringList): Boolean;
    function ResumeTorrentsAll: Boolean;
    function ResumeTorrents(const aHashes: TStringList): Boolean;

    property IsLogged: Boolean
      read FIsLogged;
    property LoginCookie: String
      read FLoginCookie;
    property APIVersion: String
      read FAPIVersion;
    property qBitTorrentVersion: String
      read FqBitTorrentVersion;
    //property RefreshID: Integer
    //  read FRefreshID;

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
    property Version: String
      read GetVersion;
  end;

implementation

uses
  fpjson, jsonparser, jsonscanner;

const
  // Current API version: 2.6.0
  csAPIPath = '/api/v2';
{$IFDEF linux}
  {$IFDEF CPUX64}
  sUserAgent = 'lazqBitTorrentWebUI/'+csVersion+' (X11; Linux x86_64;) Synapse/40.1';
  {$ELSE}
  sUserAgent = 'lazqBitTorrentWebUI/'+csVersion+' (X11; Linux i386;) Synapse/40.1';
  {$ENDIF}
{$ENDIF}
{$IFDEF windows}
  {$IFDEF CPUX64}
  sUserAgent = 'lazqBitTorrentWebUI/'+csVersion+' (Windows x86_64;) Synapse/40.1';
  {$ELSE}
  sUserAgent = 'lazqBitTorrentWebUI/'+csVersion+' (Windows i386;) Synapse/40.1';
  {$ENDIF}
{$ENDIF}
{$IFDEF darwin}
  {$IFDEF CPUX64}
  sUserAgent = 'lazqBitTorrentWebUI/'+csVersion+' (OSX x86_64;) Synapse/40.1';
  {$ELSE}
  sUserAgent = 'lazqBitTorrentWebUI/'+csVersion+' (OSX i386;) Synapse/40.1';
  {$ENDIF}
{$ENDIF}

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
  FAPIVersion := '';
  FqBitTorrentVersion := '';
  FRefreshID := 0;

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
      FAPIVersion := DoGetApiVersion;
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

function TqBitTorrentWebUI.GetVersion: String;
begin
  Result:= csVersion;
end;

procedure TqBitTorrentWebUI.Clear;
begin
  FLoginCookie := '';
  FAPIVersion := '';
  FqBitTorrentVersion := '';
  FTorrents.Clear;
end;

function TqBitTorrentWebUI.DoLogin: Boolean;
var
  sURL: String;
  sCookie: String;
  index: Integer;
  iPos: Integer;
const
  sPath = csAPIPath + '/auth/login';
begin
  Result := False;
  FHttp.Clear;
  FHttp.UserAgent := sUserAgent;
  if FPort = 80 then
  begin
    sURL := 'http://'+FHost+sPath+'?username='+FUserName+'&password='+FPassword;
  end
  else
  begin
    sURL := 'http://'+FHost+':'+IntToStr(FPort)+sPath+'?username='+FUserName+'&password='+FPassword;
  end;
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      Result := True;
      FIsLogged := True;
      sCookie := '';
      FLoginCookie := '';
      if FHttp.Cookies.Count > 0 then
      begin
        for index := 0 to Pred(FHttp.Cookies.Count) do
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
          Format('Login Cookies error: %d', [FHttp.Cookies.Count])
        );
      end;
    end
    else
    begin
      raise Exception.Create(
        Format('Login failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.DoLogout: Boolean;
var
  sURL: String;
const
  sPath = csAPIPath + '/auth/logout';
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
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      Result := True;
      FIsLogged := False;
    end
    else
    begin
      raise Exception.Create(
        Format('Logout failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.DoGetApiVersion: String;
var
  sURL: String;
  sVer: TStringStream;
const
  sPath = csAPIPath + '/app/webapiVersion';
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
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      sVer := TStringStream.Create('');
      FHttp.Document.SaveToStream(sVer);
      Result := sVer.DataString;
      sVer.Free;
    end
    else
    begin
      raise Exception.Create(
        Format('API Version failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.DoGetqBitTorrentVersion: String;
var
  sURL: String;
  sVer: TStringStream;
const
  sPath = csAPIPath + '/app/version';
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
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      sVer := TStringStream.Create('');
      FHttp.Document.SaveToStream(sVer);
      Result := sVer.DataString;
      sVer.Free;
    end
    else
    begin
      raise Exception.Create(
        Format('qBitTorrent Version failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.DoExecShutdown: Boolean;
var
  sURL: String;
const
  sPath = csAPIPath + '/app/shutdown';
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
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      Result := True;
    end
    else
    begin
      raise Exception.Create(
        Format('Shutdown failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
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

function TqBitTorrentWebUI.DoGetTorrents(const aFilter: TqBTorrentsFilter): Boolean;
var
  sURL: String;
const
  sPath = csAPIPath + '/torrents/info';
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
  if Assigned(aFilter) then
  begin
    sURL := sURL + '?' + aFilter.Filters;
  end;
  { #todo -ogcarreno : On debug get a file from sessions }
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      Result := True;
      FTorrents.LoadTorrents(FHttp.Document);
    end
    else
    begin
      raise Exception.Create(
        Format('Getting torrents failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.GetTorrents: Boolean;
begin
  if FActive then
  begin
    Result := DoGetTorrents(nil);
  end
  else
  begin
    Result := False;
    raise Exception.Create(
      'You need to set Active True first.'
    );
  end;
end;

function TqBitTorrentWebUI.GetTorrentsFiltered(const aFilter: TqBTorrentsFilter): Boolean;
begin
  if FActive then
  begin
    Result := DoGetTorrents(aFilter);
  end
  else
  begin
    Result := False;
    raise Exception.Create(
      'You need to set Active True first.'
    );
  end;
end;

function TqBitTorrentWebUI.DoGetTorrentProperties(const aHash: String): Boolean;
var
  sURL: String;
const
  sPath = csAPIPath + '/torrents/properties';
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
  sURL := sURL + '?hash=' + aHash;
  // TDOD: On debug get a file from sessions
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      Result := True;
      FTorrents.Hashes[aHash].Properties.Load(FHttp.Document);
    end
    else
    begin
      raise Exception.Create(
        Format('Getting torrent properties failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.GetTorrentProperties(const aHash: String): Boolean;
begin
  if FActive then
  begin
    Result := DoGetTorrentProperties(aHash);
  end
  else
  begin
    Result := False;
    raise Exception.Create(
      'You need to set Active True first.'
    );
  end;
end;

function TqBitTorrentWebUI.DoGetTorrentTrackers(const aHash: String): Boolean;
var
  sURL: String;
const
  sPath = csAPIPath + '/torrents/trackers';
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
  sURL := sURL + '?hash=' + aHash;
  // TDOD: On debug get a file from sessions
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      Result := True;
      FTorrents.UpdateTorrentTrackers(aHash, FHttp.Document);
    end
    else
    begin
      raise Exception.Create(
        Format('Getting torrent trackers failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.GetTorrentTrackers(const aHash: String): Boolean;
begin
  if FActive then
  begin
    Result := DoGetTorrentTrackers(aHash);
  end
  else
  begin
    Result := False;
    raise Exception.Create(
      'You need to set Active True first.'
    );
  end;
end;

function TqBitTorrentWebUI.DoGetTorrentWebSeeds(const aHash: String): Boolean;
var
  sURL: String;
const
  sPath = csAPIPath + '/torrents/webseeds';
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
  sURL := sURL + '?hash=' + aHash;
  // TDOD: On debug get a file from sessions
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      Result := True;
      FTorrents.Hashes[aHash].WebSeeds.Load(FHttp.Document);
    end
    else
    begin
      raise Exception.Create(
        Format('Getting torrent web seeds failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.GetTorrentWebSeeds(const aHash: String): Boolean;
begin
  if FActive then
  begin
    Result := DoGetTorrentWebSeeds(aHash);
  end
  else
  begin
    Result := False;
    raise Exception.Create(
      'You need to set Active True first.'
    );
  end;
end;

function TqBitTorrentWebUI.DoGetTorrentFiles(const aHash: String): Boolean;
var
  sURL: String;
const
  sPath = csAPIPath + '/torrents/files';
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
  sURL := sURL + '?hash=' + aHash;
  { #todo -ogcarreno : On debug get a file from sessions }
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      Result := True;
      FTorrents.UpdateTorrentFiles(aHash, FHttp.Document); //Hashes[aHash].Files.LoadFiles(FHttp.Document);
    end
    else
    begin
      raise Exception.Create(
        Format('Getting torrent files failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.GetTorrentFiles(const aHash: String): Boolean;
begin
  if FActive then
  begin
    Result := DoGetTorrentFiles(aHash);
  end
  else
  begin
    Result := False;
    raise Exception.Create(
      'You need to set Active True first.'
    );
  end;
end;

function TqBitTorrentWebUI.DoPauseTorrentsAll: Boolean;
var
  sURL: String;
const
  sPath = csAPIPath + '/torrents/pause';
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
  sURL := sURL + '?hashes=all';
  { #todo -ogcarreno : On debug get a file from sessions }
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      Result := True;
    end
    else
    begin
      raise Exception.Create(
        Format('Pausing all torrents failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.PauseTorrentsAll: Boolean;
begin
  if FActive then
  begin
    Result := DoPauseTorrentsAll;
  end
  else
  begin
    Result := False;
    raise Exception.Create(
      'You need to set Active True first.'
    );
  end;
end;

function TqBitTorrentWebUI.DoPauseTorrents(const aHashes: TStringList): Boolean;
var
  sURL: String;
const
  sPath = csAPIPath + '/torrents/pause';
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
  sURL := sURL + '?hashes=';
  aHashes.Delimiter:='|';
  aHashes.StrictDelimiter:= True;
  sURL:= sURL + aHashes.DelimitedText;
  { #todo -ogcarreno : On debug get a file from sessions }
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      Result := True;
    end
    else
    begin
      raise Exception.Create(
        Format('Pausing torrents failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.PauseTorrents(const aHashes: TStringList): Boolean;
begin
  if FActive then
  begin
    Result := DoPauseTorrents(aHashes);
  end
  else
  begin
    Result := False;
    raise Exception.Create(
      'You need to set Active True first.'
    );
  end;
end;

function TqBitTorrentWebUI.DoResumeTorrentsAll: Boolean;
var
  sURL: String;
const
  sPath = csAPIPath + '/torrents/resume';
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
  sURL := sURL + '?hashes=all';
  { #todo -ogcarreno : On debug get a file from sessions }
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      Result := True;
    end
    else
    begin
      raise Exception.Create(
        Format('Resuming all torrents failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.ResumeTorrentsAll: Boolean;
begin
  if FActive then
  begin
    Result := DoResumeTorrentsAll;
  end
  else
  begin
    Result := False;
    raise Exception.Create(
      'You need to set Active True first.'
    );
  end;
end;

function TqBitTorrentWebUI.DoResumeTorrents(const aHashes: TStringList): Boolean;
var
  sURL: String;
const
  sPath = csAPIPath + '/torrents/resume';
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
  sURL := sURL + '?hashes=';
  aHashes.Delimiter:='|';
  aHashes.StrictDelimiter:= True;
  sURL:= sURL + aHashes.DelimitedText;
  { #todo -ogcarreno : On debug get a file from sessions }
  if FHttp.HTTPMethod('GET', sURL) then
  begin
    if FHttp.ResultCode = 200 then
    begin
      Result := True;
    end
    else
    begin
      raise Exception.Create(
        Format('Resuming torrents failed: %d %s', [FHttp.ResultCode, FHttp.ResultString])
      );
    end;
  end
  else
  begin
    raise Exception.Create(
      Format('GET %s failed: %d %s', [sURL, FHttp.ResultCode, FHttp.ResultString])
    );
  end;
end;

function TqBitTorrentWebUI.ResumeTorrents(const aHashes: TStringList): Boolean;
begin
  if FActive then
  begin
    Result := DoResumeTorrents(aHashes);
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
