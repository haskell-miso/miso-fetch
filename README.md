# :ramen: ⚡ miso-fetch

<a href="fetch.haskell-miso.org">
  <img width="708" height="349" alt="image" src="https://github.com/user-attachments/assets/62e6e3dc-ad69-4fdd-bdff-186efc6e51eb" />
</a>

### Usage

```haskell
----------------------------------------------------------------------------
data Action
  = FetchGitHub
  | SetGitHub (Response GitHub)
  | ErrorHandler (Response MisoString)
----------------------------------------------------------------------------
app :: App Model Action
app = component emptyModel updateModel viewModel
----------------------------------------------------------------------------
updateModel :: Action -> Transition Model Action
updateModel = \case
  FetchGitHub ->
    getJSON "https://api.github.com" [] SetGitHub ErrorHandler
  SetGitHub Response {..} ->
    info ?= body
  ErrorHandler Response {..} ->
    io_ (consoleError body)
----------------------------------------------------------------------------
```

## Build and run

Install [Nix Flakes](https://nixos.wiki/wiki/Flakes), then:

```
nix develop .#wasm
make
make serve
```
