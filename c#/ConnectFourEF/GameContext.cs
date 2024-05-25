using Microsoft.EntityFrameworkCore;

public class GameState
{
    public int Id { get; set; }
    public string Board { get; set; }
    public char CurrentPlayer { get; set; }
}

public class GameContext : DbContext
{
    public DbSet<GameState> GameStates { get; set; }

    protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
    {
        optionsBuilder.UseSqlite("Data Source=connectfour.db");
    }
}
