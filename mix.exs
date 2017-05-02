defmodule Lz4.Mixfile do
  use Mix.Project

  def project do
    [app: :lz4,
     version: "0.2.4",
     elixir: ">= 1.0.3",
     compilers: [:erlang, :elixir], 
     deps: deps, 
     description: description,
     package: package]
  end

  def application, do: []

  def deps do 
    [
      {:ex_doc, ">= 0.0.0", only: :dev},
    ]
  end

  defp description do
    "LZ4 bindings for Erlang"
  end

  defp package do
    [files: ["c_src/Makefile", "c_src/*.c", "c_src/*.h", "src", "test", "README.md", "CHANGES", "rebar.config"],
     maintainers: ["SUZUKI Tetsuya"], 
     licenses: ["ISC"],
     links: %{"GitHub" => "https://github.com/szktty/erlang-lz4.git"}]
  end
end
