defmodule HubWeb.PageControllerTest do
  use HubWeb.ConnCase

  test "GET /", %{conn: conn} do
    conn = get(conn, "/")
    assert html_response(conn, 200) =~ "SewingHub"
  end
end
