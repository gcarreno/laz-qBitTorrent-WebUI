uses
  qBitTorrentWebUI;

type
  TForm1 = class(TForm)
    {...}
    Fqb: TqBitTorrentWebUI;
    {...}
  public
    {...}
    procedure GetTorrentsExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.GetTorrentsExecute(Sender: TObject);
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
        oFilter.Free;
        if Fqb.GetTorrents then
        begin
          // Do something after a successfull torrents retrival
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
