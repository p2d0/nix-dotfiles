# -*- mode: snippet -*-
# --
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace `(+yas-csharp/namespace)`
{
public class `(+yas/filename)` : IEntityTypeConfiguration<`(+yas-csharp/entity-type-configuration-base)`>
{
public void Configure(EntityTypeBuilder<`(+yas-csharp/entity-type-configuration-base)`> builder)
{
builder.ToTable("$1");
builder.HasKey(p=>p.Id);
builder.Property(p=>p.Id).HasColumnName("id").ValueGeneratedOnAdd();
builder.Property(p=>p.CreatedDateTime).HasColumnName("created_date_time").IsRequired();
builder.Property(p=>p.ModifiedDateTime).HasColumnName("modified_date_time").IsRequired(false);
$0
}
}
}
