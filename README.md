# pastagram
Scrap public instagram profile with pascal

---

- Scrap first few latest posts only
- No automatic loading for next posts

## Usage example

```delphi
uses
  pastagram;

var
  IGProfile: TIGProfile;
  IGPost: TIGPost;
  Username: string;

begin
  Write('Username: ');
  ReadLn(Username);

  IGProfile := TIGProfile.Create(Username);
  WriteLn('Fetching the data...');
  IGProfile.FetchData;

  WriteLn('Full name:');
  WriteLn(IGProfile.FullName, sLineBreak);
  WriteLn('Biography:');
  WriteLn(IGProfile.Biography, sLineBreak);

  WriteLn('Profile pic url : ', IGProfile.ProfilePicURL);
  WriteLn('Followers       : ', IGProfile.NumFollowers);
  WriteLn('Following       : ', IGProfile.NumFollowing);

  for IGPost in IGProfile.PostList do
  begin
    writeln('Media URL:', sLineBreak, IGPost.MediaURL);
    writeln('Caption:', sLineBreak, IGPost.Caption, sLineBreak);
  end;
  ReadLn;
end. 
```
