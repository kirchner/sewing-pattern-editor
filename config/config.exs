# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :hub,
  ecto_repos: [Hub.Repo]

# Configures the endpoint
config :hub, HubWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "Zr9c0uElDn4BJ+5P83+rhwvjkirfYqGiP28PSFLdTd2+y1H9JNZVkqN52BrT/JhF",
  render_errors: [view: HubWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Hub.PubSub, adapter: Phoenix.PubSub.PG2],
  live_view: [signing_salt: "1iPckzVB"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

config :ueberauth, Ueberauth,
  providers: [
    identity: {
      Ueberauth.Strategy.Identity, [
        callback_methods: ["POST"],
        uid_field: :email,
      ],
    },
    github: { Ueberauth.Strategy.Github, [default_scope: ""] },
    twitter: { Ueberauth.Strategy.Twitter, [] },
  ]

config :ueberauth, Ueberauth.Strategy.Github.OAuth,
  client_id: System.get_env("GITHUB_CLIENT_ID"),
  client_secret: System.get_env("GITHUB_CLIENT_SECRET")

config :ueberauth, Ueberauth.Strategy.Twitter.OAuth,
  consumer_key: System.get_env("TWITTER_CONSUMER_KEY"),
  consumer_secret: System.get_env("TWITTER_CONSUMER_SECRET")

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
