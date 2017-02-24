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
begin
  try
    try
      Fqb.Active := True;
      if Fqb.GetTorrents then
      begin
        // Do something after a successfull torrents retrival
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
