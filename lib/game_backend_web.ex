defmodule GameBackendWeb do
  def controller do
    quote do
      use Phoenix.Controller, namespace: GameBackendWeb

      import Plug.Conn
      import GameBackendWeb.Gettext
      alias GameBackendWeb.Router.Helpers, as: Routes
    end
  end

  def view do
    quote do
      use Phoenix.View,
        root: "lib/game_backend_web/templates",
        namespace: GameBackendWeb

      import Phoenix.Controller,
        only: [get_flash: 1, get_flash: 2, view_module: 1]

      use Phoenix.HTML
      import GameBackendWeb.ErrorHelpers
      import GameBackendWeb.Gettext
      alias GameBackendWeb.Router.Helpers, as: Routes
    end
  end

  def channel do
    quote do
      use Phoenix.Channel
      import GameBackendWeb.Gettext
    end
  end

  def router do
    quote do
      use Phoenix.Router
    end
  end

  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
