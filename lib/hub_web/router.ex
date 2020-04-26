defmodule HubWeb.Router do
  use HubWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
    plug :fetch_session
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :auth do
    plug HubWeb.Auth
  end


  scope "/auth", HubWeb do
    pipe_through :browser

    get "/:provider", AuthController, :request
    get "/:provider/callback", AuthController, :callback
  end

  scope "/auth", HubWeb do
    pipe_through [:api, :auth]

    delete "/logout", AuthController, :delete
  end

  scope "/api", HubWeb do
    pipe_through [:api, :auth]

    get "/user", UserController, :show
  end

  scope "/", HubWeb do
    pipe_through :browser

    get "/story*anything", PageController, :story
    get "/*anything", PageController, :index
  end
end
