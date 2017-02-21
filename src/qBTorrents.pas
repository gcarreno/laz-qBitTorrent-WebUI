{
  Torrent file descriptor and container

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
unit qBTorrents;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils;

type
  { TqBTorrent }
    TqBTorrent = class(TObject)
    private
      FHash: String;
      FName: String;
      FSize: Int64;
      FProgress: Double;
      FDlSpeed: Integer;
      FUpSpeed: Integer;
      FPriority: Integer;
    protected
    public
      constructor Create;
      destructor Destroy; override;

      property Hash: String
        read FHash
        write FHash;
      property Name: String
        read FName
        write FName;
      property Size: Int64
        read FSize
        write FSize;
      property Progress: Double
        read FProgress
        write FProgress;
      property DlSpeed: Integer
        read FDlSpeed
        write FDlSpeed;
      property UpSpeed: Integer
        read FUpSpeed
        write FUpSpeed;
      property Priority: Integer
        read FPriority
        write FPriority;
    end;

{ TqBTorrents }
  TqBTorrents = class(TFPObjectList)
  private
    function GetItem(Index: Integer): TqBTorrent;
    procedure SetItem(Index: Integer; AObject: TqBTorrent);
  protected
  public
    property Items[Index: Integer]: TqBTorrent
      read GetItem
      write SetItem; default;
  end;

implementation

{ TqBTorrent }

constructor TqBTorrent.Create;
begin
  //
end;

destructor TqBTorrent.Destroy;
begin
  inherited Destroy;
end;

{ TqBTorrents }

function TqBTorrents.GetItem(Index: Integer): TqBTorrent;
begin
  Result := TqBTorrent(inherited GetItem(Index));
end;

procedure TqBTorrents.SetItem(Index: Integer; AObject: TqBTorrent);
begin
  inherited SetItem(Index, AObject);
end;

end.

