unit pastagram;

{$mode objfpc}{$H+}

interface

uses
  fphttpclient,
  fpjson,
  jsonparser,
  SysUtils;

type
  TIGPost = record
    MediaURL: string;
    Caption:  string;
  end;
  TIGPostList = array of TIGPost;

  TIGProfile = class
  private
    FFetched:      boolean;
    FBiography:    string;
    FFullName:     string;
    FNumFollowers: integer;
    FNumFollowing: integer;
    FPostList:     TIGPostList;
    FProfilePicURL: string;
    FUsername:     string;
    function GetBiography: string;
    function GetFullName: string;
    function GetNumFollowers: integer;
    procedure AssertFetched;
    function GetNumFollowing: integer;
    function GetProfilePicURL: string;
  public
    constructor Create(AUsername: string);
    property Biography: string read GetBiography;
    property FullName: string read GetFullName;
    property Fetched: boolean read FFetched write FFetched;
    property NumFollowers: integer read GetNumFollowers;
    property NumFollowing: integer read GetNumFollowing;
    property ProfilePicURL: string read GetProfilePicURL;
    property PostList: TIGPostList read FPostList;
    property Username: string read FUsername;
    procedure FetchData;
  end;

implementation

function TIGProfile.GetBiography: string;
begin
  AssertFetched;
  Result := FBiography;
end;

procedure TIGProfile.AssertFetched;
begin
  Assert(Fetched, 'Please fetch the data first.');
end;

function TIGProfile.GetFullName: string;
begin
  AssertFetched;
  Result := FFullName;
end;

function TIGProfile.GetNumFollowers: integer;
begin
  AssertFetched;
  Result := FNumFollowers;
end;

function TIGProfile.GetNumFollowing: integer;
begin
  AssertFetched;
  Result := FNumFollowing;
end;

function TIGProfile.GetProfilePicURL: string;
begin
  AssertFetched;
  Result := FProfilePicURL;
end;

constructor TIGProfile.Create(AUsername: string);
begin
  FUsername := AUsername;
end;

procedure TIGProfile.FetchData;
var
  BaseURL, Suffix, url: string;
  HttpClient: TFPHTTPClient;
  HttpResultString: string;
  ProfData, MediaData: TJSONData;
  MediaCount, i, CaptionCnt: integer;
begin
  BaseURL := 'https://www.instagram.com/';
  Suffix  := '/?__a=1'; { this is the magic *shrug* }
  url     := Format('%s%s%s', [BaseURL, FUsername, Suffix]);

  HttpClient := TFPHTTPClient.Create(nil);
  HttpClient.AllowRedirect := True;
  try
    HttpResultString := HttpClient.SimpleGet(url);
    ProfData := GetJSON(HttpResultString);

    { Set fields' value }
    FBiography     := ProfData.GetPath('graphql.user.biography').AsString;
    FFullName      := ProfData.GetPath('graphql.user.full_name').AsString;
    FNumFollowing  := ProfData.GetPath('graphql.user.edge_follow.count').AsInteger;
    FNumFollowers  := ProfData.GetPath('graphql.user.edge_followed_by.count').AsInteger;
    FProfilePicURL := ProfData.GetPath('graphql.user.profile_pic_url_hd').AsString;

    { Handle IG posts data }
    MediaData  := ProfData.GetPath('graphql.user.edge_owner_to_timeline_media.edges');
    MediaCount := MediaData.Count;
    SetLength(FPostList, MediaCount);
    for i := 0 to MediaCount - 1 do
    begin
      FPostList[i].MediaURL := MediaData.Items[i].GetPath('node.display_url').AsString;

      CaptionCnt := MediaData.Items[i].GetPath('node.edge_media_to_caption.edges').Count;
      if CaptionCnt > 0 then
        FPostList[i].Caption := MediaData.Items[i].GetPath(
          'node.edge_media_to_caption.edges[0].node.text').AsString;
    end;

    { Now that everything is properly loaded }
    Fetched := True;

  finally
    HttpClient.Free;
    ProfData.Free;
  end;
end;

end.
