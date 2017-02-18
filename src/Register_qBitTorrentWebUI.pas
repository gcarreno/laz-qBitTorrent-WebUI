{
  qBitTorrentWebUI component.

  Copyright 2017 Gustavo Carreno <guscarreno@gmail.com>
}
unit Register_qBitTorrentWebUI;

{$mode objfpc}{$H+}

interface
uses
  Classes,
  SysUtils,
  LResources,
  qBitTorrentWebUI;

Procedure Register;

implementation

Procedure Register;
begin
  {$I tqbittorrentwebui_icon.lrs}
  RegisterComponents('qBitTorrent', [TqBitTorrentWebUI]);
end;

end.
