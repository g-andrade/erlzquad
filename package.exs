defmodule erlzquad.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :erlzquad,
     version: @version,
     description: "A quadtree with Z-order curve indexing",
     package: package]
  end

  defp package do
    [files: ~w(src rebar.config README.md LICENSE),
     contributors: ["Guilherme Andrade"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/g-andrade/erlzquad"}]
  end
end
