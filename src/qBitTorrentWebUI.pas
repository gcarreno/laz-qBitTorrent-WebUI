{
  qBitTorrentWebUI component.

  Copyright 2017 Gustavo Carreno <guscarreno@gmail.com>
}
unit qBitTorrentWebUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPSend;

type
{ TqBitTorrentWebUI }
  TqBitTorrentWebUI = class(TComponent)
  private
    //FActive: Boolean;
    FUserName: String;
    FPassword: String;
    FHost: String;
    FPort: Integer;
    FHttp: THTTPSend;
    FIsLogged: Boolean;
    FLoginCookie: String;

    // Authentication
    function DoLogin: Boolean;
    function DoLogout: Boolean;

    // Get methods
    function DoGetApiVersion: String;
    function DoGetMinApiVersion: String;
    function DoGetqBitTorrentVersion: String;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Authentication
    function Login: Boolean;
    function Logout: Boolean;

    // Get methods
    function GetApiVersion: String;
    function GetMinApiVersion: String;
    function GetqBitTorrentVersion: String;

    property IsLogged: Boolean
      read FIsLogged;
    property LoginCookie: String
      read FLoginCookie;
  published
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

const
  sUserAgent = 'lazqBitTorrentWebUI/0.1.0.6';

{ TqBitTorrentWebUI }

constructor TqBitTorrentWebUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUserName := 'admin';
  FPassword := 'admin';
  FHost := '127.0.0.1';
  FPort := 8080;
  FHttp := THTTPSend.Create;
  FIsLogged := False;
end;

destructor TqBitTorrentWebUI.Destroy;
begin
  FHttp.Free;
  inherited Destroy;
end;

function TqBitTorrentWebUI.Login: Boolean;
begin
  if FIsLogged then
  begin
    Result := True;
    exit;
  end;
  Result := False;
  Result := DoLogin;
  FIsLogged := Result;
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

function TqBitTorrentWebUI.Logout: Boolean;
begin
  if not FIsLogged then
  begin
    Result := True;
    exit;
  end;
  Result := False;
  Result := DoLogout;
  FIsLogged := not Result;
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
  end
  else
  begin
    raise Exception.Create(
      'Logout failed: '+IntToStr(FHttp.ResultCode)+' '+FHttp.ResultString
    );
  end;
end;

function TqBitTorrentWebUI.GetApiVersion: String;
begin
  if FIsLogged then
  begin
    Result := DoGetApiVersion;
  end
  else
  begin
    Result := '';
    raise Exception.Create(
      'You need to login first.'
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

function TqBitTorrentWebUI.GetMinApiVersion: String;
begin
  if FIsLogged then
  begin
    Result := DoGetMinApiVersion;
  end
  else
  begin
    Result := '';
    raise Exception.Create(
      'You need to login first.'
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

function TqBitTorrentWebUI.GetqBitTorrentVersion: String;
begin
  if FIsLogged then
  begin
    Result := DoGetqBitTorrentVersion;
  end
  else
  begin
    Result := '';
    raise Exception.Create(
      'You need to login first.'
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

end.
