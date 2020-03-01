defmodule HubWeb.SessionView do
  use HubWeb, :view
  alias HubWeb.UserView

  def render("show.json", %{user: user}) do
    %{data: render_one(user, UserView, "user.json")}
  end
end
