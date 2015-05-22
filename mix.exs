defmodule Lz4.Mixfile do
  use Mix.Project

  def project do
    [app: :lz4,
     version: "0.2.2",
     elixir: ">= 1.0.3",
     description: description,
     package: package]
  end

  def application, do: []

  defp description do
    "LZ4 bindings for Erlang" <>
    "This library uses source code of LZ4 from https://github.com/Cyan4973/lz4."
  end

  defp package do
    [files: ["c_src", "src", "test", "README.md", "CHANGES", "Makefile", "rebar", "rebar.config"],
     contributors: ["SUZUKI Tetsuya", "Lei Ting"],
     licenses: ["ISC"],
     links: %{"GitHub" => "https://github.com/szktty/erlang-lz4.git"}]
  end
end
