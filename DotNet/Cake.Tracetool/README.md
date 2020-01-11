# Cake.Tracetool

This is a thin wrapper around the Tracetool library.

## Usage 

The intention of this Cake plugin is to encourage development teams to store their passwords in a vault rather than checked into source control. 

The plugin has been kept very simple/single responsibility, in order to allow its use in as many situations as possible. The plugin will never attempt to write the retrieved data to configuration files, environment variables or anywhere else - this is for the script writer to perform using one of the other more suitable cake plugins.

## Example

```csharp
#Addin "Cake.Tracetool"

...

```

