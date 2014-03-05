package troop;

import engagement.Engagable;

abstract public class AbstractTroop implements TroopBase, Engagable
{
    protected int queuedDamage = 0;
	protected int strength;
	protected int str_inc;
	protected int upgrades = 0; // this will only be used if we expand to a unit creator
	
	// this makes sense for a unit creator
	// make a unit at basic strength, and then upgrade it later
	public AbstractTroop()
	{
		strength = str_inc;
	}

	// shortcut creator for testing purposes and project version
	public AbstractTroop(int strength)
	{
		this.strength = strength;
	}
	
	public int getUpgrades()
	{
		return upgrades;
	}
	
	public int getStrength()
	{
		return strength;
	}
	
	public void upgradeStrength()
	{
		strength += str_inc;
		upgrades++;
	}
	
	// undo button; makes more sense for unit creator
	public void unupgradeStrength()
    {
        strength -= str_inc;
        upgrades--;
    }
	
	@Override
	public void queueDamage(int damage)
    {
        if(damage >= 0) queuedDamage += damage;
    }
	
	@Override
	public void sufferQueuedDamage()
    {
        if(queuedDamage >= 0) strength -= queuedDamage;
        queuedDamage = 0;
    }
	
	public void sufferDamage(int damage)
	{
		if(damage >= 0) strength -= damage;
	}
	
	public String toString()
	{
	    return ""+strength;
	}
}
