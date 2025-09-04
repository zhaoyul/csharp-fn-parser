using System;

public class Greeter
{
    public void SayHello()
    {
        Console.WriteLine("Enter your name: ");
        var name = Console.ReadLine();
        if (string.IsNullOrEmpty(name))
        {
            LogWarning("Name cannot be empty.");
            return;
        }
        GreetUser(name);
    }

    private void GreetUser(string name)
    {
        Console.WriteLine($"Hello, {name}!");
    }

    private void LogWarning(string message)
    {
        // A logging function
        System.Diagnostics.Debug.WriteLine(message);
    }
}
