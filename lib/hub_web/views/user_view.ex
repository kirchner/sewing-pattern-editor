defmodule HubWeb.UserView do
  use HubWeb, :view
  alias HubWeb.UserView

  def render("show.json", %{user: user}) do
    %{data: render_one(user, UserView, "user.json")}
  end

  def render("user.json", %{user: _user}) do
    %{}
  end
end
