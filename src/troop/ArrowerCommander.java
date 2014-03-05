package troop;

public class ArrowerCommander extends Arrower implements Commander
{
    protected int command = 5;
    
    public ArrowerCommander()
    {
        super();
    }
    
    public ArrowerCommander(int command)
    {
        super();
        this.command = command;
    }
    
    @Override
    public void upgradeCommand()
    {
        upgrades++;
        command++;
    }
    
    @Override
    public int getCommand()
    {
        return command;
    }
    
    @Override
    public String toString()
    {
        return "("+command+", "+strength+")";
    }
}