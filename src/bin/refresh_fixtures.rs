use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use reqwest::{Client, StatusCode};
use scraper::Html;
use std::fs;
use std::path::{Path, PathBuf};
use woerterbuch::http;
use woerterbuch::models::{Source, SourceResult, UrlValue};
use woerterbuch::sources;

const WORDS: &[&str] = &[
    "Bank",
    "Haus",
    "Nixdaexistiert",
    "Wolke",
    "Zaun",
    "springen",
    "verlieben",
];

#[derive(Debug, Parser)]
#[command(name = "refresh-fixtures")]
#[command(about = "Download source fixtures and generate expected JSON outputs")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Download,
    Render,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Download => download_all().await,
        Command::Render => render_all(),
    }
}

async fn download_all() -> Result<()> {
    let client = http::build_client()?;

    for word in WORDS {
        download_duden(&client, word).await?;
        download_dwds(&client, word).await?;
        download_wiktionary(&client, word).await?;
        download_openthesaurus(&client, word).await?;
    }

    Ok(())
}

fn render_all() -> Result<()> {
    for word in WORDS {
        write_expected(Source::Duden, word, &render_duden(word)?)?;
        write_expected(Source::Dwds, word, &render_dwds(word)?)?;
        write_expected(Source::Wiktionary, word, &render_wiktionary(word)?)?;
        write_expected(Source::Openthesaurus, word, &render_openthesaurus(word)?)?;
    }

    Ok(())
}

async fn download_duden(client: &Client, word: &str) -> Result<()> {
    let dir = fixture_dir(Source::Duden, word);
    fs::create_dir_all(&dir)?;

    let entry_url = sources::duden::build_url(word);
    let search_url = sources::duden::build_search_url(word);

    let (entry_status, entry_body) = fetch_response(client, &entry_url).await?;
    write_text_file(dir.join("entry.status"), &entry_status.as_u16().to_string())?;
    write_text_file(dir.join("entry.html"), &entry_body)?;
    write_text_file(dir.join("entry.url"), &entry_url)?;

    let (search_status, search_body) = fetch_response(client, &search_url).await?;
    write_text_file(
        dir.join("search.status"),
        &search_status.as_u16().to_string(),
    )?;
    write_text_file(dir.join("search.html"), &search_body)?;
    write_text_file(dir.join("search.url"), &search_url)?;

    if word == "Bank" {
        let urls = sources::duden::parse_search_results_for_fixture(
            &Html::parse_document(&search_body),
            word,
        );
        for (index, url) in urls.iter().enumerate() {
            let (status, body) = fetch_response(client, url).await?;
            let slot = index + 1;
            write_text_file(
                dir.join(format!("entry-{slot}.status")),
                &status.as_u16().to_string(),
            )?;
            write_text_file(dir.join(format!("entry-{slot}.html")), &body)?;
            write_text_file(dir.join(format!("entry-{slot}.url")), url)?;
        }
    }

    Ok(())
}

async fn download_dwds(client: &Client, word: &str) -> Result<()> {
    let dir = fixture_dir(Source::Dwds, word);
    fs::create_dir_all(&dir)?;

    let url = sources::dwds::build_url(word);
    let (status, body) = fetch_response(client, &url).await?;
    write_text_file(dir.join("page.status"), &status.as_u16().to_string())?;
    write_text_file(dir.join("page.html"), &body)?;
    write_text_file(dir.join("page.url"), &url)?;
    Ok(())
}

async fn download_wiktionary(client: &Client, word: &str) -> Result<()> {
    let dir = fixture_dir(Source::Wiktionary, word);
    fs::create_dir_all(&dir)?;

    let api_url = sources::wiktionary::build_api_url(word);
    let page_url = sources::wiktionary::build_page_url(word);
    let (status, body) = fetch_response(client, &api_url).await?;
    write_text_file(dir.join("page.status"), &status.as_u16().to_string())?;
    write_text_file(dir.join("page.html"), &body)?;
    write_text_file(dir.join("page.url"), &page_url)?;
    Ok(())
}

async fn download_openthesaurus(client: &Client, word: &str) -> Result<()> {
    let dir = fixture_dir(Source::Openthesaurus, word);
    fs::create_dir_all(&dir)?;

    let api_url = sources::openthesaurus::build_api_url(word);
    let page_url = sources::openthesaurus::build_page_url(word);
    let (status, body) = fetch_response(client, &api_url).await?;
    write_text_file(dir.join("page.status"), &status.as_u16().to_string())?;
    write_text_file(dir.join("page.json"), &body)?;
    write_text_file(dir.join("page.url"), &page_url)?;
    Ok(())
}

fn render_duden(word: &str) -> Result<SourceResult> {
    let dir = fixture_dir(Source::Duden, word);
    let entry_url = read_text_file(dir.join("entry.url"))?;
    let entry_status = read_status_file(dir.join("entry.status"))?;
    let entry_html = read_text_file(dir.join("entry.html"))?;

    if entry_status.is_success() {
        let entry = sources::duden::parse_entry_for_fixture(word, &entry_url, &entry_html, 1)
            .with_context(|| format!("Duden entry fixture did not parse for {word}"))?;
        return Ok(SourceResult::ok(
            Source::Duden,
            Some(UrlValue::One(entry_url)),
            vec![entry],
        ));
    }

    let search_html = read_text_file(dir.join("search.html"))?;
    let urls =
        sources::duden::parse_search_results_for_fixture(&Html::parse_document(&search_html), word);

    if urls.is_empty() {
        return Ok(sources::duden::no_match_result_for_fixture());
    }

    let mut entries = Vec::new();
    for (index, url) in urls.iter().enumerate() {
        let html = read_text_file(dir.join(format!("entry-{}.html", index + 1)))?;
        let entry = sources::duden::parse_entry_for_fixture(word, url, &html, index + 1)
            .with_context(|| format!("Duden homograph fixture did not parse for {word}"))?;
        entries.push(entry);
    }

    Ok(SourceResult::ok(
        Source::Duden,
        Some(if urls.len() == 1 {
            UrlValue::One(urls[0].clone())
        } else {
            UrlValue::Many(urls)
        }),
        entries,
    ))
}

fn render_dwds(word: &str) -> Result<SourceResult> {
    let dir = fixture_dir(Source::Dwds, word);
    let url = read_text_file(dir.join("page.url"))?;
    let html = read_text_file(dir.join("page.html"))?;
    sources::dwds::parse(word, &url, &html)
}

fn render_wiktionary(word: &str) -> Result<SourceResult> {
    let dir = fixture_dir(Source::Wiktionary, word);
    let status = read_status_file(dir.join("page.status"))?;
    let url = read_text_file(dir.join("page.url"))?;
    let body = read_text_file(dir.join("page.html"))?;

    if status == StatusCode::NOT_FOUND {
        return Ok(sources::wiktionary::not_found_result_for_fixture(
            word, &url,
        ));
    }

    sources::wiktionary::parse(word, &url, &body)
}

fn render_openthesaurus(word: &str) -> Result<SourceResult> {
    let dir = fixture_dir(Source::Openthesaurus, word);
    let url = read_text_file(dir.join("page.url"))?;
    let body = read_text_file(dir.join("page.json"))?;
    sources::openthesaurus::parse(word, &url, &body)
}

async fn fetch_response(client: &Client, url: &str) -> Result<(StatusCode, String)> {
    let response = client.get(url).send().await?;
    let status = response.status();
    let body = response.text().await?;
    Ok((status, body))
}

fn fixture_dir(source: Source, word: &str) -> PathBuf {
    Path::new("tests")
        .join("fixtures")
        .join(source_name(source))
        .join(word)
}

fn expected_path(source: Source, word: &str) -> PathBuf {
    Path::new("tests")
        .join("expected")
        .join(source_name(source))
        .join(format!("{word}.json"))
}

fn source_name(source: Source) -> &'static str {
    match source {
        Source::Duden => "duden",
        Source::Dwds => "dwds",
        Source::Wiktionary => "wiktionary",
        Source::Openthesaurus => "openthesaurus",
    }
}

fn write_expected(source: Source, word: &str, result: &SourceResult) -> Result<()> {
    let path = expected_path(source, word);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    write_text_file(path, &serde_json::to_string_pretty(result)?)
}

fn write_text_file(path: impl AsRef<Path>, content: &str) -> Result<()> {
    fs::write(path, content)?;
    Ok(())
}

fn read_text_file(path: impl AsRef<Path>) -> Result<String> {
    let value = fs::read_to_string(path.as_ref())?;
    Ok(trim_trailing_newline(value))
}

fn trim_trailing_newline(mut value: String) -> String {
    while value.ends_with('\n') || value.ends_with('\r') {
        value.pop();
    }
    value
}

fn read_status_file(path: impl AsRef<Path>) -> Result<StatusCode> {
    let raw = read_text_file(path)?;
    let code: u16 = raw
        .parse()
        .with_context(|| format!("invalid status code: {raw}"))?;
    StatusCode::from_u16(code).with_context(|| format!("unsupported status code: {code}"))
}
