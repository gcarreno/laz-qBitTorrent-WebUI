uses
  qBitTorrentWebUI;

type
  TForm1 = class(TForm)
    {...}
    Fqb: TqBitTorrentWebUI;
    {...}
  public
    {...}
    procedure GetTorrentFilesExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.GetTorrentFilesExecute(Sender: TObject);
var
  oFilter: TqBTorrentsFilter;
begin
  try
    try
      Fqb.Active := True;
      oFilter := TqBTorrentsFilter.Create;
      try
        oFilter
          .withFilter('all')
          .withSort('priority')
          .withLimit(10);
        if Fqb.GetTorrentsFiltered(oFilter) then
        begin
          if Fqb.Items.Count > 0 then
          begin
            if Fqb.GetTorrentFiles(Fqb[0].Hash) then
            begin
              // Do something after a successfull torrent's files retrieval
            end;
          end;
        end;
      finally
        oFilter.Free;
      end;
    except
      on E:Exception do
      begin
        // If torrents retrieval fails it will raise an exception
      end;
    end;
  finally
    Fqb.Active := False;
  end;
end;
