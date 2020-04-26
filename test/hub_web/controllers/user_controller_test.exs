defmodule HubWeb.UserControllerTest do
  use HubWeb.ConnCase

  alias Hub.Accounts
  alias Hub.Accounts.User

  @create_attrs %{}

  def fixture(:user) do
    {:ok, user} = Accounts.create_user(@create_attrs)
    user
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "show" do
    test "returns unauthorized, when user is not logged in", %{conn: conn} do
      conn = get(conn, Routes.user_path(conn, :show))
      assert json_response(conn, 401)
    end
  end
end
