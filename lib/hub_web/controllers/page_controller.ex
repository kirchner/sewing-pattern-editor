defmodule HubWeb.PageController do
  use HubWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html", %{layout: {HubWeb.LayoutView, "app.html"}})
  end

  def story(conn, _params) do
    render(conn, "stories.html", %{layout: {HubWeb.LayoutView, "stories.html"}})
  end
end
