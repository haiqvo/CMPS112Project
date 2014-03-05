package troop;

import engagement.EngagableSingle;

abstract public class Troop implements EngagableSingle
{
	protected int strength;
	protected int str_inc;
	protected int upgrades = 0; // this will only be used if we expand to a unit creator
	
	// this makes sense for a unit creator
	// make a unit at basic strength, and then upgrade it later
	public Troop()
	{
		strength = str_inc;
	}

	// shortcut creator for testing purposes and project version
	Troop(int strength)
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
	
	public void sufferDamage(int damage)
	{
		if(damage >= 0) strength -= damage;
	}
	
	public String toString()
	{
	    return ""+strength;
	}
}
