// ライブラリファイルを抽出し、プロジェクトルートディレクトリ直下にpomファイルの雛形を作成する。
// 各プロジェクトで共通のライブラリを使用し、バージョンが競合する場合、統一するか問い合わせる。
// このスクリプトだけでは必要な情報はすべて埋まらないので、必ず雛形を手動で修正すること。

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Net
open System.Web
open System.Xml.Linq

/// jarなどのライブラリファイルを指定するファイルパターン
let libraryFilePattern = "*.jar"
/// pomファイル名
let pomFileName = "pom.xml"
/// 作成するpom雛形ファイル名
let pomTemplateFileName = "pom_template.xml"
/// プロジェクトルートを表すファイル名
let projectRootFileName = ".project"
/// ライブラリファイルが格納されているディレクトリかどうかを判定する。
let isLibraryDirectory = (=) "lib"
/// セントラルリポジトリを検索するサイト
let repoWebSite = "http://mvnrepository.com"
/// セントラルリポジトリを検索するクエリ
let repoSearchQuery = "/search.html?query="

/// ライブラリ基本情報。
type LibraryBaseInfo =
/// Sunのライブラリ(ProjectPath * ArtifactId * GroupId)
| Sun of (string * string * string)
/// サードパーティのライブラリ(ProjectPath * ArtifactId * Version)
| ThirdParty of (string * string * string option)

/// バージョンの決め方
type Versioning =
/// 検索サイトでの最新バージョン
| Latest
/// 既存ライブラリのバージョンから選択、または最新バージョン、または手動入力
| Choice
/// 元のライブラリのバージョンを変えない
| Original

/// ライブラリの情報を表すレコード
type LibraryInfo = {
    ProjectPath : string
    GroupId : string option
    ArtifactId : string
    Version : string option
}

/// Sunのライブラリ名（＝artifactId）に対応するgroupIdの辞書
let sunLibraryDict =
    dict [
        "activation", "javax.activation"
        "j2ee", "javax.j2ee"
        "jdo", "javax.jdo"
        "jms", "javax.jms"
        "mail", "javax.mail"
        "ejb", "javax.persistence"
        "connector", "javax.resource"
        "connector-api", "javax.resource"
        "jaas", "javax.security"
        "jacc", "javax.security"
        "servlet-api", "javax.servlet"
        "jsp-api", "javax.servlet"
        "jstl", "javax.servlet"
        "jdbc-stdext", "javax.sql"
        "jta", "javax.transaction"
        "jaxrpc", "javax.xml"
        "portlet-api", "javax.portlet"
        "jndi", "javax.naming"
    ]

/// ディレクトリの種類を判別する。
let (|Library|ProjectRoot|NotCovered|Other|) dir =
    let children dir = dir |> Directory.EnumerateDirectories |> Seq.cache
    match dir |> Directory.EnumerateFiles |> Seq.map Path.GetFileName |> Seq.cache with
    | files when files |> Seq.exists ((=) pomFileName) -> NotCovered
    | files when files |> Seq.exists ((=) projectRootFileName) -> ProjectRoot
    | _ when Path.GetFileName dir |> isLibraryDirectory -> Library
    | _ -> Other

/// 標準入力から入力を受け、条件に合致した場合のみ出力する。条件に合致しない場合は再入力させる。
let rec input message condition =
    printfn "%s" message
    match Console.ReadLine() |> condition with Some result -> result | _ -> input message condition

/// 指定したディレクトリ以下のすべてのライブラリファイルとプロジェクト名を取得する。
let getAllLibraries dir =
    let rec getAllLibraries projectName isDescendant dir =
        let children pj desc = Directory.EnumerateDirectories >> Seq.collect (getAllLibraries pj desc)
        seq {
            printfn "%s を探索" dir
            match dir, projectName, isDescendant with
            | NotCovered, _, _ ->
                printfn "%sが既に存在したのでこのディレクトリ以下は無視します" pomFileName
            | ProjectRoot, _, _ ->
                dir |> Path.GetFileName |> printfn "プロジェクト %s の処理開始"
                yield! dir |> children (Some dir) false
            | Library, Some pj, _ | _, Some pj, true ->
                yield! Directory.EnumerateFiles(dir, libraryFilePattern) |> Seq.map (fun libName ->(libName, pj))
                printfn "このディレクトリに存在するライブラリファイルを抽出しました"
                yield! dir |> children projectName true
            | _ -> yield! dir |> children projectName false }
    getAllLibraries None false dir

/// ライブラリファイルとプロジェクト名からライブラリ基本情報を取得する。
let getLibraryBaseInfo libraryFileName projectPath =
    let file = libraryFileName |> Path.GetFileNameWithoutExtension
    match sunLibraryDict.TryGetValue file with
    | true, groupId -> Sun(projectPath, file, groupId)
    | _ ->
        let m = Regex.Match(file, @"^(?<name>.*?)(-(?<version>\d.*?))?(-sources?)?(-javadoc)?$")
        ThirdParty(projectPath, m.Groups.["name"].Value, match m.Groups.["version"].Value with "" -> None | v -> Some v)

/// artifactIdに該当するライブラリをセントラルリポジトリ検索サイトから検索する。
let searchInCentralRepo artifactId versioning =
    /// ライブラリ情報を手動入力する。
    let inputLibraryInfo (page : string) versioning =
        let condition = function "y" | "Y" -> Some "y" | "w" | "W" -> Some "w" | "n" | "N" -> Some "n" | _ -> None
        let getGroupIdVersion() =
            ["ArtifactId", "そのまま"; "GroupId", "空欄"] @ (match versioning with Latest -> ["Version", "空欄"] | _ -> [])
            |> List.map (fun mes -> input (mes ||> sprintf "%sを入力（入力しない場合は%s）") Some |> function "" -> None | r -> Some r)
            |> function [a; g; v] -> a, g, v | [a; g] -> a, g, None | _ -> None, None, None
        match input "手動で入力しますか？（y:入力する w:検索サイトを開いてから入力する n:入力しない）" condition with
        | "y" -> getGroupIdVersion()
        | "w" ->
            System.Diagnostics.Process.Start page |> ignore
            getGroupIdVersion()
        | _ -> None, None, None

    try
        use webClient = new WebClient()
        let libraryPageUrl = "\"" + artifactId + "\"" |> HttpUtility.UrlEncode |> sprintf "%s%s%s" repoWebSite repoSearchQuery
        let libraryPage = libraryPageUrl |> webClient.DownloadString
        Regex.Matches(libraryPage, @"<p class=""result"">.+?</p>", RegexOptions.Singleline)
        |> Seq.cast<Match> |> Seq.tryPick (fun m ->
            let m = Regex.Match(m.Value, @"<a[^>]+>(?<group>[^<]+)</a>\s*&#187\s*<a\s+href=""(?<page>[^""]+)""\s*class=""result-link"">(?<artifact>[^<]+)</a>\s*</p>")
            if m.Success && m.Groups.["artifact"].Value = artifactId then
                Some(m.Groups.["page"].Value, m.Groups.["group"].Value)
            else None)
        |> function
        | Some(page, groupId) ->
            let artifact, group, version =
                match versioning with
                | Latest | Choice ->
                    let versionPage = sprintf "%s%s" repoWebSite page |> webClient.DownloadString
                    match Regex.Match(versionPage,
                                      @"<a class=""versionbutton[^>]*>(?<version>(\d){1,7}(\D[^<]*|))</a></td>[^<]\s*<td>release</td>",
                                      RegexOptions.Singleline) with
                    | m when m.Success ->
                        let version = m.Groups.["version"].Value
                        printfn "ライブラリ %s のGroupId(%s)とVersion(%s)を取得しました。" artifactId groupId version
                        None, Some groupId, Some version
                    | _ ->
                        printfn "ライブラリ %s のGroupId(%s)を取得しましたが、Versionは取得できませんでした。" artifactId groupId
                        inputLibraryInfo libraryPageUrl versioning
                | Original ->
                    printfn "ライブラリ %s のGroupId(%s)を取得しました。" artifactId groupId
                    None, Some groupId, None
            Some(artifact, group, version)
        | _ ->
            printfn "ライブラリ %s の情報を取得できませんでした。" artifactId
            let artifact, group, version = inputLibraryInfo libraryPageUrl versioning
            Some(artifact, group, version)
    with
    | :? WebException ->
        printfn "ライブラリ %s の検索中に、検索サイトへの接続でエラーが発生しました。" artifactId
        None

/// ライブラリ情報を確定する。
let setGroupIdAndVersion versioning libraries =
    let infos = libraries |> Seq.map ((<||) getLibraryBaseInfo) |> Seq.cache
    let idVersionDic =
        infos |> Seq.choose (function ThirdParty info -> Some info | _ -> None) |> Seq.groupBy (fun (_, a, _) -> a)
        |> Seq.map (fun (a, infos) ->
            infos |> Seq.groupBy (fun (_, _, v) -> v) |> Seq.sortBy fst
            |> Seq.map (fun (v, infos) -> v, (infos |> Seq.map (fun (p, _, _) -> Path.GetFileName p) |> Seq.distinct))
            |> Seq.toArray |> fun infoGroup -> a, infoGroup)
        |> dict
    let memo = Dictionary()
    infos |> Seq.sortBy (function Sun(_, a, _) -> a | ThirdParty(_, a, _) -> a) |> Seq.map (function
        | Sun(projectPath, artifactId, groupId) ->
            { ProjectPath = projectPath; GroupId = Some groupId; ArtifactId = artifactId; Version = None }
        | ThirdParty(projectPath, artifactId, version) when memo.ContainsKey artifactId ->
            let groupId, ver = memo.[artifactId]
            { ProjectPath = projectPath; GroupId = groupId; ArtifactId = artifactId;
              Version = match versioning with Original -> version | _ -> ver }
        | ThirdParty(projectPath, artifactId, version) ->
            let setArtifactId newArtifact = defaultArg newArtifact artifactId
            match versioning with
            | Latest ->
                match searchInCentralRepo artifactId versioning with
                | Some(artifact, group, version) ->
                    let artifactId = setArtifactId artifact
                    memo.Add(artifactId, (group, version))
                    { ProjectPath = projectPath; GroupId = group; ArtifactId = artifactId; Version = version }
                | _ ->
                    memo.Add(artifactId, (None, None))
                    { ProjectPath = projectPath; GroupId = None; ArtifactId = artifactId; Version = None }
            | Choice ->
                let artifact, groupId, latestVersion =
                    match searchInCentralRepo artifactId versioning with
                    | Some(a, g, v) -> a, g, v | _ -> None, None, None
                let artifactId = setArtifactId artifact
                printfn "ライブラリ %s のバージョンを指定してください。\n0 : 任意の値を入力" artifactId
                idVersionDic.[artifactId] |> Array.iteri (fun i (ver, projects) ->
                    projects |> String.concat "," |> printfn "%d : %s (%s)"  (i + 1) (defaultArg ver "バージョンなし"))
                match latestVersion with
                | Some v -> printfn "%d : 検索サイトでの最新バージョン（%s）" (idVersionDic.[artifactId].Length + 1) v
                | _ -> ()
                let rec input() =
                    match Console.ReadLine() |> Int32.TryParse with
                    | true, 0 ->
                        printfn "バージョンを入力してください。"
                        Console.ReadLine() |> Some
                    | true, i when 0 < i && i <= idVersionDic.[artifactId].Length ->
                        idVersionDic.[artifactId].[i - 1] |> fst
                    | true, i when latestVersion.IsSome && i = idVersionDic.[artifactId].Length + 1 ->
                        latestVersion
                    | _ -> input()
                let version = input()
                memo.Add(artifactId, (groupId, version))
                { ProjectPath = projectPath; GroupId = groupId; ArtifactId = artifactId; Version = version }
            | Original ->
                let artifactId, groupId =
                    match searchInCentralRepo artifactId versioning with
                    | Some(artifact, group, _) -> setArtifactId artifact, group
                    | _ -> artifactId, None
                memo.Add(artifactId, (groupId, version))
                { ProjectPath = projectPath; GroupId = groupId; ArtifactId = artifactId; Version = version })

/// ライブラリ情報からpomファイルの雛形を出力する。
let createPomTemplate dir libraryInfos =
    let indent level = String.replicate level "\t"
    let xe level tag value = sprintf "%s<%s>%s</%s>\n" (indent level) tag value tag
    let xes level tag value = sprintf "%s<%s>\n%s%s</%s>\n" (indent level) tag value (indent level) tag
    let xcomment level = sprintf "%s<!-- %s -->\n" (indent level)
    let todo = xcomment 1 "TODO : 追加情報を入力してください"
    let orDefault x = defaultArg x "<!-- TODO : 適切な値を手動で入力してください -->"
    let dependencies =
        libraryInfos |> Seq.map (fun { GroupId = g; ArtifactId = a; Version = v } -> orDefault g, a, orDefault v)
        |> Seq.map (fun (g, a, v) -> xe 3 "groupId" g + xe 3 "artifactId" a + xe 3 "version" v |> xes 2 "dependency")
        |> String.concat "" |> xes 1 "dependencies"
    let fileContent = [todo; dependencies; todo] |> String.concat "" |> xes 0 "project"
    let fileName = Path.Combine(dir, pomTemplateFileName)
    if File.Exists fileName |> not || 
            (printfn "%s には%sが既に存在します。上書きしますか？(y/n) " dir pomTemplateFileName
             match Console.ReadLine() with "y" | "Y" -> true | _ -> false) then
        File.WriteAllText(fileName, fileContent, Encoding.UTF8)
        printfn "%s を作成しました。" fileName

/// メイン処理
[<EntryPoint>]
let main _ =
    try
        let directoryCondition dir =
            if Path.IsPathRooted dir |> not then printfn "フルパスではありません"; None
            elif Directory.Exists dir |> not then printfn "指定したディレクトリが存在しません"; None else
            (if dir.[dir.Length - 1] = '\\' then dir.[.. dir.Length - 2] else dir) |> Some
        let dir = input "探索を開始するディレクトリをフルパスで指定" directoryCondition
        let onlyRootMessage = "ライブラリをpomにまとめる単位は？\n1 : 探索開始ディレクトリ直下をすべて1つにまとめる\n2 : 各プロジェクトごとにまとめる"
        let onlyRootCondition = function "1" -> Some true | "2" -> Some false | _ -> None
        let isOnlyRoot = input onlyRootMessage onlyRootCondition
        let libMessage =
            "ライブラリのバージョンの扱いは？\n1 : 最新のバージョンを検索サイトから取得\n2 : 現存するライブラリからバージョンを選択、または手動入力"
            + if isOnlyRoot then "" else "\n3 : 元のまま"
        let versioningCondition = function
            | "1" -> Some Latest | "2" -> Some Choice | "3" when isOnlyRoot |> not -> Some Original | _ -> None
        let versioning = input libMessage versioningCondition
        printfn "探索開始"
        let infos = dir |> getAllLibraries |> setGroupIdAndVersion versioning
        if isOnlyRoot then infos |> Seq.distinctBy (fun { GroupId = g; ArtifactId = a; Version = v } -> g, a, v) |> createPomTemplate dir
        else infos |> Seq.distinct |> Seq.groupBy (fun { ProjectPath = p } -> p) |> Seq.iter ((<||) createPomTemplate)
        0
    with
    | ex ->
        ex |> string |> eprintfn "エラーが発生しました。\n%s"
        1
