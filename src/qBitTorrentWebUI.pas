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

    function DoLogin: Boolean;
    function DoLogout: Boolean;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Login: Boolean;
    function Logout: Boolean;

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
      write FPort;
    // TODO: Add some Notifications for HTTP access:
    // Like Login and such
  end;

implementation

{ TqBitTorrentWebUI }

constructor TqBitTorrentWebUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUserName := 'admin';
  FPassword := 'admin';
  FHost := 'localhost';
  FPort := 9090;
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

function TqBitTorrentWebUI.DoLogin: Boolean;
var
  sURL: String;
  sDoc: String;
  sCookie: String;
  index: Integer;
  iPos: Integer;
begin
  Result := False;
  FHttp.Clear;
  FHttp.Headers.Add('Content-Type: application/x-www-form-urlencoded');
  sDoc := 'username='+FUserName+'&password='+FPassword;
  FHttp.Document.WriteAnsiString(sDoc);
  if FPort = 80 then
  begin
    sURL := 'http://'+FHost+'/login'
  end
  else
  begin
    sURL := 'http://'+FHost+':'+IntToStr(FPort)+'/login';
  end;
  FHttp.HTTPMethod('POST', sURL);
  if FHttp.ResultCode = 200 then
  begin
    Result := True;
    sCookie := '';
    for index := 0 to FHttp.Cookies.Count - 1 do
    begin
      sCookie := FHttp.Cookies[index];
      iPos := Pos('SID=', sCookie);
      if iPos > 0 then
      begin
        FLoginCookie := Copy(sCookie, iPos+3, Length(sCookie));
        Break;
      end;
    end;
    FLoginCookie := sCookie;
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
begin
  Result := False;
  FHttp.Clear;
  if FPort = 80 then
  begin
    sURL := 'http://'+FHost+'/logout'
  end
  else
  begin
    sURL := 'http://'+FHost+':'+IntToStr(FPort)+'/logout';
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

end.
