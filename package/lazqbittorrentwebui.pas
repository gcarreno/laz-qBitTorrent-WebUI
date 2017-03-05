{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazqBitTorrentWebUI;

{$warn 5023 off : no warning about unused units}
interface

uses
  Register_qBitTorrentWebUI, qBitTorrentWebUI, qBUtils, qBTorrents, 
  qBTorrentsFilters, qBTorrentsProperties, qBTorrentsTrackers, 
  qBTorrentsWebSeeds, qBTorrentsFiles, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Register_qBitTorrentWebUI', @Register_qBitTorrentWebUI.Register
    );
end;

initialization
  RegisterPackage('lazqBitTorrentWebUI', @Register);
end.
