{
  qBitTorrentWebUI component.

  Copyright 2017 Gustavo Carreno <guscarreno@gmail.com>
}
unit qBitTorrentWebUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
{ TqBitTorrentWebUI }
  TqBitTorrentWebUI = class(TComponent)
  private
    //FActive: Boolean;
    FUserName: String;
    FPassword: String;
    FHost: String;
    FPort: Integer;
    // TODO: Have the HTTP object here
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
  // TODO: Instantiate the HTTP object
end;

destructor TqBitTorrentWebUI.Destroy;
begin
  // TODO: Destroy the HTTP object
  inherited Destroy;
end;

end.
